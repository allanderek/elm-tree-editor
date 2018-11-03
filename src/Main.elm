module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
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
    always Sub.none


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
    | InsertDown Tree


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

                ( expr, Node kind left up right ) ->
                    ( expr, Node kind left up (tree :: right) )

        InsertDown tree ->
            case location of
                ( Leaf _, _ ) ->
                    location

                ( Section kind children, path ) ->
                    ( Section kind (tree :: children), path )


view : Model -> Browser.Document Msg
view model =
    { title = "Tree source editing with Elm"
    , body =
        [ header model.location
        , viewLocation model.location
        ]
    }


header : Location -> Html Msg
header ( expr, path ) =
    let
        button title msg =
            Html.button
                [ Attributes.class "control"
                , Events.onClick msg
                ]
                [ text title ]

        disabledButton title =
            Html.button
                [ Attributes.class "control"
                , Attributes.disabled True
                ]
                [ text title ]

        insertLeft =
            let
                title =
                    "InsertLeft"

                insert e =
                    button title <| EditorAction <| InsertLeft e
            in
            case path of
                Top ->
                    disabledButton title

                Node Decl _ _ _ ->
                    disabledButton title

                Node Appl _ _ _ ->
                    insert <| Leaf "e"

                Node Let _ _ _ ->
                    insert <| Section Decl [ Leaf "c", Leaf "3" ]
    in
    div
        [ Attributes.class "controls" ]
        [ button "Up" <| EditorAction MoveUp
        , button "Left" <| EditorAction MoveLeft
        , button "Right" <| EditorAction MoveRight
        , button "Down" <| EditorAction MoveDown
        , insertLeft
        , button "InsertRight" <| EditorAction <| InsertRight (Leaf "e")
        , button "InsertDown" <| EditorAction <| InsertDown (Leaf "e")
        ]


viewKind : TreeKind -> List (Html msg) -> Html msg
viewKind kind viewed =
    let
        viewInvalid children =
            div
                [ Attributes.class "invalid-expression" ]
                [ text "Invalid expression"
                , div [] children
                ]
    in
    case ( kind, viewed ) of
        ( Decl, [ pattern, expr ] ) ->
            div
                [ Attributes.class "declaration" ]
                [ div
                    [ Attributes.class "pattern" ]
                    [ pattern ]
                , text " = "
                , expr
                ]

        ( Let, children ) ->
            case List.Extra.unconsLast children of
                Nothing ->
                    viewInvalid children

                Just ( last, decls ) ->
                    div
                        [ Attributes.class "let" ]
                        [ text "let"
                        , div
                            [ Attributes.class "declarations" ]
                            decls
                        , text "in"
                        , div
                            [ Attributes.class "let-payload" ]
                            [ last ]
                        ]

        ( Appl, children ) ->
            div
                [ Attributes.class "application" ]
                children

        ( _, children ) ->
            viewInvalid children


viewTree : Tree -> Html msg
viewTree tree =
    case tree of
        Leaf s ->
            text s

        Section kind children ->
            viewKind kind <| List.map viewTree children


viewPath : Html msg -> Path -> Html msg
viewPath viewed path =
    case path of
        Top ->
            div
                [ Attributes.class "top" ]
                [ viewed ]

        Node kind left up right ->
            let
                children =
                    (List.reverse <| List.map viewTree left)
                        ++ (viewed :: List.map viewTree right)

                child =
                    viewKind kind children
            in
            viewPath child up


viewHighlighted : Tree -> Html Msg
viewHighlighted tree =
    case tree of
        Leaf s ->
            Html.input
                [ Attributes.class "leaf-input"
                , Attributes.id leafBoxId
                , Events.onInput LeafInput
                , Attributes.value s
                ]
                []

        Section _ _ ->
            div
                [ Attributes.class "highlighted" ]
                [ viewTree tree ]


viewLocation : Location -> Html Msg
viewLocation ( expr, path ) =
    viewPath (viewHighlighted expr) path
