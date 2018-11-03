module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (Element, el, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attributes
import Json.Decode as Decode
import List.Extra
import Task
import Url


main : Program ProgramFlags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


subscriptions : Model -> Sub Msg
subscriptions =
    always keySubscription


keySubscription : Sub Msg
keySubscription =
    let
        keyDecoder : Decode.Decoder Msg
        keyDecoder =
            let
                toMsg k =
                    case keyToEditorAction k of
                        Nothing ->
                            NoOp

                        Just e ->
                            EditorAction e
            in
            Decode.map toMsg <| Decode.field "key" Decode.string

        keyToEditorAction : String -> Maybe EditorAction
        keyToEditorAction string =
            case string of
                "ArrowLeft" ->
                    Just MoveLeft

                "ArrowRight" ->
                    Just MoveRight

                "ArrowUp" ->
                    Just MoveUp

                "ArrowDown" ->
                    Just MoveDown

                _ ->
                    Nothing
    in
    Browser.Events.onKeyPress keyDecoder


type alias ProgramFlags =
    ()


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , location : Location
    }


type alias Location =
    ( Tree, Path )


type Tree
    = Leaf String
    | Section TreeKind (List Tree)


type TreeKind
    = Let
    | Decl
    | Appl


type Path
    = Top
    | Node TreeKind (List Tree) Path (List Tree)


initialLocation : Location
initialLocation =
    ( Section
        Let
        [ Section Decl [ Leaf "a", Leaf "1" ]
        , Section Decl [ Leaf "b", Leaf "2" ]
        , Section Appl [ Leaf "add", Leaf "a", Leaf "b" ]
        ]
    , Top
    )


init : ProgramFlags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        initialModel =
            { key = key
            , url = url
            , location = initialLocation
            }

        initialCommand =
            Cmd.none
    in
    ( initialModel, initialCommand )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | EditorAction EditorAction
    | LeafInput String
    | NoOp


type EditorAction
    = MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown
    | InsertLeft Tree
    | InsertRight Tree


leafBoxId : String
leafBoxId =
    "leaf-box"


focusLeafBox : Cmd Msg
focusLeafBox =
    Task.attempt (always NoOp) (Browser.Dom.focus leafBoxId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            let
                command =
                    case urlRequest of
                        Browser.Internal url ->
                            Nav.pushUrl model.key <| Url.toString url

                        Browser.External href ->
                            Nav.load href
            in
            ( model, command )

        UrlChanged url ->
            withCommands { model | url = url } []

        NoOp ->
            withCommands model []

        EditorAction action ->
            let
                newModel =
                    { model | location = updateLocation action model.location }

                command =
                    case newModel.location of
                        ( Leaf _, _ ) ->
                            focusLeafBox

                        _ ->
                            Cmd.none
            in
            ( newModel, command )

        LeafInput content ->
            let
                newModel =
                    case model.location of
                        ( Leaf s, path ) ->
                            { model | location = ( Leaf content, path ) }

                        _ ->
                            model
            in
            withCommands newModel []


withCommands : model -> List (Cmd msg) -> ( model, Cmd msg )
withCommands model commands =
    ( model, Cmd.batch commands )


updateLocation : EditorAction -> Location -> Location
updateLocation action location =
    case action of
        MoveLeft ->
            case location of
                ( _, Top ) ->
                    location

                ( _, Node _ [] _ _ ) ->
                    location

                ( expr, Node kind (l :: left) up right ) ->
                    ( l, Node kind left up (expr :: right) )

        MoveRight ->
            case location of
                ( _, Top ) ->
                    location

                ( _, Node _ _ _ [] ) ->
                    location

                ( expr, Node kind left up (r :: right) ) ->
                    ( r, Node kind (expr :: left) up right )

        MoveUp ->
            case location of
                ( _, Top ) ->
                    location

                ( expr, Node kind left up right ) ->
                    let
                        newExpr =
                            Section kind <| List.reverse left ++ (expr :: right)
                    in
                    ( newExpr, up )

        MoveDown ->
            case location of
                ( Leaf _, _ ) ->
                    location

                ( Section _ [], _ ) ->
                    location

                ( Section kind (f :: rest), path ) ->
                    ( f, Node kind [] path rest )

        InsertLeft tree ->
            case location of
                ( _, Top ) ->
                    location

                ( expr, Node kind left up right ) ->
                    ( expr, Node kind (tree :: left) up right )

        InsertRight tree ->
            case location of
                ( _, Top ) ->
                    location

                ( _, Node Let _ _ [] ) ->
                    location

                ( expr, Node kind left up right ) ->
                    ( expr, Node kind left up (tree :: right) )


view : Model -> Browser.Document Msg
view model =
    let
        body =
            Element.column
                [ Element.width Element.fill
                , Element.spacing 10
                , Element.padding 10
                ]
                [ header model.location
                , viewLocation model.location
                ]
    in
    { title = "Tree source editing with Elm"
    , body = [ Element.layout [] body ]
    }


themeColor1 : Element.Color
themeColor1 =
    Element.rgb255 104 175 195


themeColor2 : Element.Color
themeColor2 =
    Element.rgb255 34 91 120


themeColor3 : Element.Color
themeColor3 =
    Element.rgb255 240 142 89


themeColor4 : Element.Color
themeColor4 =
    Element.rgb255 125 53 26


header : Location -> Element Msg
header ( expr, path ) =
    let
        button title mMsg =
            let
                opacity =
                    case mMsg == Nothing of
                        True ->
                            0.4

                        False ->
                            1.0
            in
            Input.button
                [ Background.color themeColor1
                , Border.color themeColor2
                , Border.width 1
                , Element.padding 5
                , Border.rounded 5
                , Element.alpha opacity
                ]
                { onPress = mMsg
                , label = text title
                }

        action =
            Just << EditorAction

        moveUp =
            let
                title =
                    "up"
            in
            case path of
                Top ->
                    button title Nothing

                Node _ _ _ _ ->
                    button title <| action MoveUp

        moveDown =
            let
                title =
                    "Down"
            in
            case expr of
                Leaf _ ->
                    button title Nothing

                Section _ [] ->
                    button title Nothing

                Section _ _ ->
                    button title <| action MoveDown

        moveLeft =
            let
                title =
                    "Left"
            in
            case path of
                Top ->
                    button title Nothing

                Node _ [] _ _ ->
                    button title Nothing

                Node _ _ _ _ ->
                    button title <| action MoveLeft

        moveRight =
            let
                title =
                    "Right"
            in
            case path of
                Top ->
                    button title Nothing

                Node _ _ _ [] ->
                    button title Nothing

                Node _ _ _ _ ->
                    button title <| action MoveRight

        insertLeft =
            let
                title =
                    "InsertLeft"

                insert e =
                    button title <| action <| InsertLeft e
            in
            case path of
                Top ->
                    button title Nothing

                Node Decl _ _ _ ->
                    button title Nothing

                Node Appl _ _ _ ->
                    insert <| Leaf "e"

                Node Let _ _ _ ->
                    insert <| Section Decl [ Leaf "c", Leaf "3" ]

        insertRight =
            let
                title =
                    "InsertRight"

                insert e =
                    button title <| action <| InsertRight e
            in
            case path of
                Top ->
                    button title Nothing

                Node Decl _ _ _ ->
                    button title Nothing

                Node Appl _ _ _ ->
                    insert <| Leaf "e"

                Node Let _ _ [] ->
                    button title Nothing

                Node Let _ _ _ ->
                    insert <| Section Decl [ Leaf "d", Leaf "4" ]
    in
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spaceEvenly
        ]
        [ moveUp
        , moveDown
        , moveLeft
        , moveRight
        , insertLeft
        , insertRight
        ]


indentElement : Element.Attr decorative msg
indentElement =
    Element.moveRight 10


viewKind : TreeKind -> List (Element msg) -> Element msg
viewKind kind viewed =
    let
        viewInvalid children =
            Element.column
                [ classAttribute "invalid-expression" ]
                (text "Invalid expression" :: children)
    in
    case ( kind, viewed ) of
        ( Decl, [ pattern, expr ] ) ->
            Element.column
                [ classAttribute "declaration" ]
                [ Element.row
                    [ Element.spacing 8 ]
                    [ el [ Element.width Element.fill ] pattern
                    , viewPunctuation "="
                    ]
                , el [ indentElement ] expr
                ]

        ( Let, children ) ->
            case List.Extra.unconsLast children of
                Nothing ->
                    viewInvalid children

                Just ( _, [] ) ->
                    viewInvalid children

                Just ( last, decls ) ->
                    Element.column
                        [ classAttribute "let" ]
                        [ viewKeyword "let"
                        , Element.column
                            [ Element.spacing 10
                            , indentElement
                            ]
                            decls
                        , viewKeyword "in"
                        , el
                            [ classAttribute "let-payload" ]
                            last
                        ]

        ( Appl, children ) ->
            Element.row
                [ Element.spacing 8 ]
                children

        ( _, children ) ->
            viewInvalid children


viewKeyword : String -> Element msg
viewKeyword word =
    el
        [ Font.color themeColor1 ]
        (text word)


viewPunctuation : String -> Element msg
viewPunctuation symbol =
    el
        [ Font.color themeColor3
        , Element.padding 3
        ]
        (text symbol)


viewTree : Tree -> Element msg
viewTree tree =
    case tree of
        Leaf s ->
            text s

        Section kind children ->
            viewKind kind <| List.map viewTree children


viewPath : Element msg -> Path -> Element msg
viewPath viewed path =
    case path of
        Top ->
            el
                [ classAttribute "top" ]
                viewed

        Node kind left up right ->
            let
                children =
                    (List.reverse <| List.map viewTree left)
                        ++ (viewed :: List.map viewTree right)

                child =
                    viewKind kind children
            in
            viewPath child up


viewHighlighted : Tree -> Element Msg
viewHighlighted tree =
    case tree of
        Leaf s ->
            Input.text
                [ classAttribute "leaf-input"
                , idAttribute leafBoxId
                , Element.width Element.fill
                ]
                { onChange = LeafInput
                , text = s
                , placeholder = Nothing
                , label = Input.labelHidden ""
                }

        Section _ _ ->
            el
                [ Border.width 2
                , Border.color themeColor2
                , Element.padding 3
                ]
                (viewTree tree)


viewLocation : Location -> Element Msg
viewLocation ( expr, path ) =
    viewPath (viewHighlighted expr) path


idAttribute : String -> Element.Attribute msg
idAttribute s =
    Element.htmlAttribute <| Attributes.id s


classAttribute : String -> Element.Attribute msg
classAttribute s =
    Element.htmlAttribute <| Attributes.class s
