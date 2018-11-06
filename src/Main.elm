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
    { current : Tree
    , path : Path
    }


type Tree
    = ExprTree Expr
    | PatternTree Pattern
    | DeclTree Declaration


type Expr
    = Leaf String
    | Apply (List Expr)



--| Let (List Declaration) Expr


type alias Declaration =
    { pattern : Pattern
    , expr : Tree
    }


type Pattern
    = NamePattern String


type Path
    = Top
    | ApplyPath (List Expr) Path (List Expr)



-- | LetDecl (List Declaration) Path (List Declaration) Expr
-- | LetExpr (List Declaration) Path


initialLocation : Location
initialLocation =
    { path = Top
    , current = ExprTree <| Apply [ Leaf "a", Leaf "b", Leaf "c", Leaf "d" ]
    }


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
    | PromoteLet


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
                    case newModel.location.current of
                        ExprTree (Leaf _) ->
                            focusLeafBox

                        _ ->
                            Cmd.none
            in
            ( newModel, command )

        LeafInput content ->
            let
                newModel =
                    case model.location.current of
                        ExprTree (Leaf s) ->
                            let
                                location =
                                    model.location

                                newLocation =
                                    { location | current = ExprTree (Leaf content) }
                            in
                            { model | location = newLocation }

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
            case location.path of
                Top ->
                    location

                ApplyPath [] _ _ ->
                    location

                ApplyPath (l :: left) path right ->
                    case location.current of
                        ExprTree current ->
                            { current = ExprTree l
                            , path = ApplyPath left path (current :: right)
                            }

                        PatternTree _ ->
                            location

                        DeclTree _ ->
                            location

        MoveRight ->
            case location.path of
                Top ->
                    location

                ApplyPath _ _ [] ->
                    location

                ApplyPath left path (r :: right) ->
                    case location.current of
                        ExprTree current ->
                            { current = ExprTree r
                            , path = ApplyPath (current :: left) path right
                            }

                        PatternTree _ ->
                            location

                        DeclTree _ ->
                            location

        MoveUp ->
            case location.path of
                Top ->
                    location

                ApplyPath left path right ->
                    case location.current of
                        ExprTree expr ->
                            let
                                args =
                                    List.concat
                                        [ List.reverse left
                                        , [ expr ]
                                        , right
                                        ]
                            in
                            { path = path
                            , current = ExprTree <| Apply args
                            }

                        DeclTree _ ->
                            location

                        PatternTree _ ->
                            location

        MoveDown ->
            case location.current of
                ExprTree (Leaf _) ->
                    location

                ExprTree (Apply []) ->
                    -- Obviously this should never happen, even Apply [e] shouldn't happen
                    -- we could inforce this in the type system, eg. `Apply Tree Tree` so you're only
                    -- ever applying a function one argument and what looks like many is just a tree,
                    -- which we could then navigate cleverly. This I have to admit is tempting. Another
                    -- possibility is Apply Tree Tree (List Tree) which makes sure you always have a function
                    -- and at least one argument but you can have multiple. I quite like this. It does mean
                    -- though that we require 3 paths. ApplyFun, ApplyArg, ApplyRest
                    location

                ExprTree (Apply (first :: rest)) ->
                    { current = ExprTree first
                    , path = ApplyPath [] location.path rest
                    }

                DeclTree _ ->
                    location

                PatternTree _ ->
                    location

        InsertLeft tree ->
            case location.path of
                Top ->
                    location

                ApplyPath left path right ->
                    case location.current of
                        ExprTree current ->
                            { current = tree
                            , path = ApplyPath left path (current :: right)
                            }

                        PatternTree _ ->
                            location

                        DeclTree _ ->
                            location

        InsertRight tree ->
            case location.path of
                Top ->
                    location

                ApplyPath left path right ->
                    case location.current of
                        ExprTree current ->
                            { current = tree
                            , path = ApplyPath (current :: left) path right
                            }

                        PatternTree _ ->
                            location

                        DeclTree _ ->
                            location

        PromoteLet ->
            location


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
header location =
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
            case location.path of
                Top ->
                    button title Nothing

                ApplyPath _ _ _ ->
                    button title <| action MoveUp

        moveDown =
            let
                title =
                    "Down"
            in
            case location.current of
                ExprTree (Leaf _) ->
                    button title Nothing

                ExprTree (Apply []) ->
                    button title Nothing

                ExprTree (Apply (_ :: _)) ->
                    button title <| action MoveDown

                PatternTree _ ->
                    button title Nothing

                DeclTree _ ->
                    button title Nothing

        moveLeft =
            let
                title =
                    "Left"
            in
            case location.path of
                Top ->
                    button title Nothing

                ApplyPath [] _ _ ->
                    button title Nothing

                ApplyPath (_ :: _) _ _ ->
                    button title <| action MoveLeft

        moveRight =
            let
                title =
                    "Right"
            in
            case location.path of
                Top ->
                    button title Nothing

                ApplyPath _ _ [] ->
                    button title Nothing

                ApplyPath _ _ (_ :: _) ->
                    button title <| action MoveRight

        insertLeft =
            let
                title =
                    "InsertLeft"

                insert e =
                    button title <| action <| InsertLeft e
            in
            case location.path of
                Top ->
                    button title Nothing

                ApplyPath _ _ _ ->
                    insert <| ExprTree (Leaf "l")

        insertRight =
            let
                title =
                    "InsertRight"

                insert e =
                    button title <| action <| InsertRight e
            in
            case location.path of
                Top ->
                    button title Nothing

                ApplyPath _ _ _ ->
                    insert <| ExprTree (Leaf "r")

        promoteLet =
            let
                title =
                    "Let"
            in
            button title Nothing
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
        , promoteLet
        ]


indentElement : Element.Attr decorative msg
indentElement =
    Element.moveRight 10



{- }
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
                   [ Element.padding 10 ]
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
-}


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
        ExprTree expr ->
            viewExpr expr

        DeclTree _ ->
            text "Not done decls yet."

        PatternTree _ ->
            text "Not done patterns yet"


viewExpr : Expr -> Element msg
viewExpr expr =
    case expr of
        Leaf s ->
            text s

        Apply children ->
            viewApply <| List.map viewExpr children


viewApply : List (Element msg) -> Element msg
viewApply =
    Element.wrappedRow
        [ Element.spacing 4 ]


viewPath : Element msg -> Path -> Element msg
viewPath viewed path =
    case path of
        Top ->
            el
                [ classAttribute "top" ]
                viewed

        ApplyPath left up right ->
            let
                children =
                    (List.reverse <| List.map viewExpr left)
                        ++ (viewed :: List.map viewExpr right)

                child =
                    viewApply children
            in
            viewPath child up


viewHighlighted : Tree -> Element Msg
viewHighlighted tree =
    case tree of
        ExprTree (Leaf s) ->
            Input.text
                [ classAttribute "leaf-input"
                , idAttribute leafBoxId
                , Element.width Element.shrink
                ]
                { onChange = LeafInput
                , text = s
                , placeholder = Nothing
                , label = Input.labelHidden ""
                }

        _ ->
            el
                [ Border.width 2
                , Border.rounded 5
                , Border.color themeColor2
                , Element.padding 10
                ]
                (viewTree tree)


viewLocation : Location -> Element Msg
viewLocation location =
    viewPath (viewHighlighted location.current) location.path


idAttribute : String -> Element.Attribute msg
idAttribute s =
    Element.htmlAttribute <| Attributes.id s


classAttribute : String -> Element.Attribute msg
classAttribute s =
    Element.htmlAttribute <| Attributes.class s
