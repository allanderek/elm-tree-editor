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
    | Let (List Declaration) Expr


type alias Declaration =
    { pattern : Pattern
    , expr : Expr
    }


type Pattern
    = NamePattern String


type Path
    = Top
    | ApplyPath (List Expr) Path (List Expr)
    | LetDecl (List Declaration) Path (List Declaration) Expr
    | LetExpr (List Declaration) Path


initialLocation : Location
initialLocation =
    { path = Top
    , current =
        ExprTree
            (Let
                [ { pattern = NamePattern "a", expr = Leaf "1" }
                , { pattern = NamePattern "b", expr = Leaf "2" }
                , { pattern = NamePattern "c", expr = Leaf "3" }
                , { pattern = NamePattern "d", expr = Leaf "4" }
                ]
                (Apply [ Leaf "a", Leaf "b", Leaf "c", Leaf "d" ])
            )
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

                LetDecl [] _ _ _ ->
                    location

                LetDecl (l :: left) path right expr ->
                    case location.current of
                        DeclTree d ->
                            { current = DeclTree l
                            , path = LetDecl left path (d :: right) expr
                            }

                        PatternTree _ ->
                            location

                        ExprTree _ ->
                            location

                LetExpr [] _ ->
                    location

                LetExpr (d :: declarations) path ->
                    case location.current of
                        ExprTree current ->
                            { current = DeclTree d
                            , path = LetDecl declarations path [] current
                            }

                        DeclTree _ ->
                            location

                        PatternTree _ ->
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

                LetDecl left path [] expr ->
                    case location.current of
                        DeclTree d ->
                            { current = ExprTree expr
                            , path = LetExpr (d :: left) path
                            }

                        ExprTree _ ->
                            location

                        PatternTree _ ->
                            location

                LetDecl left path (r :: right) expr ->
                    case location.current of
                        DeclTree d ->
                            { current = DeclTree r
                            , path = LetDecl (d :: left) path right expr
                            }

                        PatternTree _ ->
                            location

                        ExprTree _ ->
                            location

                LetExpr _ _ ->
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

                LetDecl left path right expr ->
                    case location.current of
                        DeclTree d ->
                            let
                                declarations =
                                    List.reverse left ++ (d :: right)
                            in
                            { current = ExprTree <| Let declarations expr
                            , path = path
                            }

                        PatternTree _ ->
                            location

                        ExprTree _ ->
                            location

                LetExpr declarations path ->
                    case location.current of
                        ExprTree e ->
                            { current = ExprTree <| Let declarations e
                            , path = path
                            }

                        PatternTree _ ->
                            location

                        DeclTree _ ->
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

                ExprTree (Let [] expr) ->
                    -- Another one that should not ever happen, but my guess is it could
                    -- in any case it's relatively easy to move down into the expression.
                    { current = ExprTree expr
                    , path = LetExpr [] location.path
                    }

                ExprTree (Let (d :: declarations) expr) ->
                    { current = DeclTree d
                    , path = LetDecl [] location.path declarations expr
                    }

                DeclTree _ ->
                    location

                PatternTree (NamePattern _) ->
                    location

        InsertLeft tree ->
            case location.path of
                Top ->
                    location

                ApplyPath left path right ->
                    case location.current of
                        ExprTree current ->
                            case tree of
                                ExprTree _ ->
                                    { current = tree
                                    , path = ApplyPath left path (current :: right)
                                    }

                                DeclTree _ ->
                                    location

                                PatternTree _ ->
                                    location

                        PatternTree _ ->
                            location

                        DeclTree _ ->
                            location

                LetDecl left path right expr ->
                    case location.current of
                        DeclTree current ->
                            case tree of
                                DeclTree _ ->
                                    { current = tree
                                    , path = LetDecl left path (current :: right) expr
                                    }

                                ExprTree _ ->
                                    location

                                PatternTree _ ->
                                    location

                        PatternTree _ ->
                            location

                        ExprTree _ ->
                            location

                LetExpr declarations path ->
                    case location.current of
                        ExprTree current ->
                            case tree of
                                DeclTree _ ->
                                    { current = tree
                                    , path = LetDecl declarations path [] current
                                    }

                                PatternTree _ ->
                                    location

                                ExprTree _ ->
                                    location

                        DeclTree _ ->
                            location

                        PatternTree _ ->
                            location

        InsertRight tree ->
            case location.path of
                Top ->
                    location

                ApplyPath left path right ->
                    case location.current of
                        ExprTree current ->
                            case tree of
                                ExprTree _ ->
                                    { current = tree
                                    , path = ApplyPath (current :: left) path right
                                    }

                                PatternTree _ ->
                                    location

                                DeclTree _ ->
                                    location

                        PatternTree _ ->
                            location

                        DeclTree _ ->
                            location

                LetDecl left path right expr ->
                    case location.current of
                        DeclTree current ->
                            case tree of
                                DeclTree _ ->
                                    { current = tree
                                    , path = LetDecl (current :: left) path right expr
                                    }

                                PatternTree _ ->
                                    location

                                ExprTree _ ->
                                    location

                        PatternTree _ ->
                            location

                        ExprTree _ ->
                            location

                LetExpr _ _ ->
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

                mMessage =
                    case location.path of
                        Top ->
                            Nothing

                        ApplyPath _ _ _ ->
                            action MoveUp

                        LetDecl _ _ _ _ ->
                            action MoveUp

                        LetExpr _ _ ->
                            action MoveUp
            in
            button title mMessage

        moveDown =
            let
                title =
                    "Down"

                mMessage =
                    case location.current of
                        ExprTree (Leaf _) ->
                            Nothing

                        ExprTree (Apply []) ->
                            Nothing

                        ExprTree (Apply (_ :: _)) ->
                            action MoveDown

                        ExprTree (Let _ _) ->
                            action MoveDown

                        PatternTree _ ->
                            Nothing

                        DeclTree _ ->
                            Nothing
            in
            button title mMessage

        moveLeft =
            let
                title =
                    "Left"

                mMessage =
                    case location.path of
                        Top ->
                            Nothing

                        ApplyPath [] _ _ ->
                            Nothing

                        ApplyPath (_ :: _) _ _ ->
                            action MoveLeft

                        LetDecl [] _ _ _ ->
                            Nothing

                        LetDecl (_ :: _) _ _ _ ->
                            action MoveLeft

                        LetExpr _ _ ->
                            action MoveLeft
            in
            button title mMessage

        moveRight =
            let
                title =
                    "Right"

                mMessage =
                    case location.path of
                        Top ->
                            Nothing

                        ApplyPath _ _ [] ->
                            Nothing

                        ApplyPath _ _ (_ :: _) ->
                            action MoveRight

                        LetDecl _ _ _ _ ->
                            action MoveRight

                        LetExpr _ _ ->
                            Nothing
            in
            button title mMessage

        insertLeft =
            let
                title =
                    "InsertLeft"

                mMessage =
                    case location.path of
                        Top ->
                            Nothing

                        ApplyPath _ _ _ ->
                            action <| InsertLeft <| ExprTree (Leaf "l")

                        LetDecl _ _ _ _ ->
                            action <| InsertLeft <| DeclTree { pattern = NamePattern "_", expr = Leaf "e" }

                        LetExpr _ _ ->
                            action <| InsertLeft <| DeclTree { pattern = NamePattern "_", expr = Leaf "e" }
            in
            button title mMessage

        insertRight =
            let
                title =
                    "InsertRight"

                mMessage =
                    case location.path of
                        Top ->
                            Nothing

                        ApplyPath _ _ _ ->
                            action <| InsertRight <| ExprTree (Leaf "r")

                        LetDecl _ _ _ _ ->
                            action <| InsertLeft <| DeclTree { pattern = NamePattern "_", expr = Leaf "e" }

                        LetExpr _ _ ->
                            Nothing
            in
            button title mMessage

        promoteLet =
            let
                title =
                    "Let"

                mMessage =
                    Nothing
            in
            button title mMessage
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


           ( Let, children ) ->
               case List.Extra.unconsLast children of
                   Nothing ->
                       viewInvalid children

                   Just ( _, [] ) ->
                       viewInvalid children

                   Just ( last, decls ) ->


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

        Let declarations inExpr ->
            viewLet (List.map viewDeclaration declarations) (viewExpr inExpr)


viewLet : List (Element msg) -> Element msg -> Element msg
viewLet declarations expr =
    Element.column
        [ classAttribute "let" ]
        [ viewKeyword "let"
        , Element.column
            [ Element.spacing 10
            , indentElement
            ]
            declarations
        , viewKeyword "in"
        , el
            [ classAttribute "let-payload" ]
            expr
        ]


viewDeclaration : Declaration -> Element msg
viewDeclaration decl =
    Element.column
        [ Element.padding 10 ]
        [ Element.row
            [ Element.spacing 8 ]
            [ el
                [ Element.width Element.fill
                , Font.color themeColor4
                ]
                (viewPattern decl.pattern)
            , viewPunctuation "="
            ]
        , el
            [ indentElement ]
            (viewExpr decl.expr)
        ]


viewPattern : Pattern -> Element msg
viewPattern pattern =
    case pattern of
        NamePattern s ->
            text s


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

        LetDecl left up right expr ->
            let
                declarations =
                    (List.reverse <| List.map viewDeclaration left)
                        ++ (viewed :: List.map viewDeclaration right)

                child =
                    viewLet declarations <| viewExpr expr
            in
            viewPath child up

        LetExpr declarations up ->
            let
                child =
                    viewLet (List.map viewDeclaration declarations) viewed
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
