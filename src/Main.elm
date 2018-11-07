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


type ExprPath
    = Top
    | ApplyPath (List Expr) ExprPath (List Expr)
    | LetExpr (List Declaration) ExprPath
    | DeclExpr Pattern DeclPath


type DeclPath
    = LetDecl (List Declaration) ExprPath (List Declaration) Expr


type PatternPath
    = DeclPattern DeclPath Expr


type Location
    = ExprLocation Expr ExprPath
    | DeclLocation Declaration DeclPath
    | PatternLocation Pattern PatternPath


initialLocation : Location
initialLocation =
    let
        expr =
            Let
                [ { pattern = NamePattern "a", expr = Leaf "1" }
                , { pattern = NamePattern "b", expr = Leaf "2" }
                , { pattern = NamePattern "c", expr = Leaf "3" }
                , { pattern = NamePattern "d", expr = Leaf "4" }
                ]
                (Apply [ Leaf "a", Leaf "b", Leaf "c", Leaf "d" ])
    in
    ExprLocation expr Top


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
    | InsertLeft
    | InsertRight
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
                    case newModel.location of
                        ExprLocation (Leaf _) _ ->
                            focusLeafBox

                        PatternLocation (NamePattern _) _ ->
                            focusLeafBox

                        _ ->
                            Cmd.none
            in
            ( newModel, command )

        LeafInput content ->
            let
                newModel =
                    case model.location of
                        ExprLocation (Leaf _) path ->
                            let
                                newLocation =
                                    ExprLocation (Leaf content) path
                            in
                            { model | location = newLocation }

                        PatternLocation (NamePattern _) path ->
                            let
                                newLocation =
                                    PatternLocation (NamePattern content) path
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
            case location of
                ExprLocation expr path ->
                    case path of
                        Top ->
                            location

                        ApplyPath [] _ _ ->
                            location

                        ApplyPath (l :: left) up right ->
                            ExprLocation l <| ApplyPath left up (expr :: right)

                        LetExpr [] _ ->
                            location

                        LetExpr (l :: left) up ->
                            DeclLocation l <| LetDecl left up [] expr

                        DeclExpr pattern up ->
                            PatternLocation pattern <| DeclPattern up expr

                DeclLocation declaration path ->
                    case path of
                        LetDecl [] _ _ _ ->
                            location

                        LetDecl (l :: left) up right expr ->
                            DeclLocation l <| LetDecl left up (declaration :: right) expr

                PatternLocation pattern path ->
                    location

        MoveRight ->
            case location of
                PatternLocation pattern path ->
                    case path of
                        DeclPattern up expr ->
                            ExprLocation expr <| DeclExpr pattern up

                DeclLocation declaration path ->
                    case path of
                        LetDecl left up [] expr ->
                            ExprLocation expr <| LetExpr (declaration :: left) up

                        LetDecl left up (r :: right) expr ->
                            DeclLocation r <| LetDecl (declaration :: left) up right expr

                ExprLocation expr path ->
                    case path of
                        Top ->
                            location

                        ApplyPath _ _ [] ->
                            location

                        ApplyPath left up (r :: right) ->
                            ExprLocation r <| ApplyPath (expr :: left) up right

                        LetExpr _ _ ->
                            location

                        DeclExpr _ _ ->
                            location

        MoveUp ->
            case location of
                PatternLocation pattern path ->
                    case path of
                        DeclPattern up expr ->
                            let
                                declaration =
                                    { pattern = pattern
                                    , expr = expr
                                    }
                            in
                            DeclLocation declaration up

                DeclLocation declaration path ->
                    case path of
                        LetDecl left up right inExpr ->
                            let
                                declarations =
                                    upList left declaration right

                                expr =
                                    Let declarations inExpr
                            in
                            ExprLocation expr up

                ExprLocation expr path ->
                    case path of
                        Top ->
                            location

                        ApplyPath left up right ->
                            let
                                args =
                                    upList left expr right
                            in
                            ExprLocation (Apply args) up

                        LetExpr declarations up ->
                            let
                                letExpr =
                                    Let (List.reverse declarations) expr
                            in
                            ExprLocation letExpr up

                        DeclExpr pattern up ->
                            let
                                declaration =
                                    { pattern = pattern
                                    , expr = expr
                                    }
                            in
                            DeclLocation declaration up

        MoveDown ->
            case location of
                PatternLocation pattern up ->
                    case pattern of
                        NamePattern _ ->
                            location

                DeclLocation declaration up ->
                    -- Note this is a break from convention we're going into the expression
                    -- which is kind of not the first child of the declaration, but my argument is
                    -- that often you will wish to modify the expression in a declaration without
                    -- modifying the name.
                    ExprLocation declaration.expr <| DeclExpr declaration.pattern up

                ExprLocation expr up ->
                    case expr of
                        Leaf _ ->
                            location

                        Apply [] ->
                            location

                        Apply (first :: rest) ->
                            ExprLocation first <| ApplyPath [] up rest

                        Let [] inExpr ->
                            -- This should not happen but I guess it's kind of possible if someone is deleting declarations
                            -- the question is whether, when someone deletes the last declaration we just coalasce the expression or not.
                            -- We could actually just do this regardless of how many declarations there are. I suspect at some point I might
                            -- have MoveDownLeft and MoveDownRight.
                            ExprLocation inExpr <| LetExpr [] up

                        Let (first :: rest) inExpr ->
                            DeclLocation first <| LetDecl [] up rest inExpr

        InsertLeft ->
            case location of
                PatternLocation _ _ ->
                    location

                DeclLocation declaration path ->
                    case path of
                        LetDecl left up right inExpr ->
                            let
                                newDeclaration =
                                    { pattern = NamePattern "l"
                                    , expr = Leaf "1"
                                    }
                            in
                            DeclLocation newDeclaration <|
                                LetDecl left up (declaration :: right) inExpr

                ExprLocation expr path ->
                    case path of
                        Top ->
                            location

                        ApplyPath left up right ->
                            ExprLocation (Leaf "1") <|
                                ApplyPath left up (expr :: right)

                        LetExpr declarations up ->
                            let
                                newDeclaration =
                                    { pattern = NamePattern "l"
                                    , expr = Leaf "1"
                                    }
                            in
                            ExprLocation expr <| LetExpr (newDeclaration :: declarations) up

                        DeclExpr _ _ ->
                            location

        InsertRight ->
            case location of
                PatternLocation _ _ ->
                    location

                DeclLocation declaration path ->
                    case path of
                        LetDecl left up right inExpr ->
                            let
                                newDeclaration =
                                    { pattern = NamePattern "r"
                                    , expr = Leaf "2"
                                    }
                            in
                            DeclLocation newDeclaration <|
                                LetDecl (declaration :: left) up right inExpr

                ExprLocation expr path ->
                    case path of
                        Top ->
                            location

                        ApplyPath left up right ->
                            let
                                newExpr =
                                    Leaf "e"
                            in
                            ExprLocation newExpr <|
                                ApplyPath (expr :: left) up right

                        LetExpr _ _ ->
                            -- In theory we could interpret this as 'insert a new declaration at the bottom of the list of declarations'
                            -- I suspect this is something of a common task, it's easy enough from here anyway you simply go right->insertRight.
                            -- In general I suspect adding a new declaration to the inner most enclosing 'let' even if you're not actually
                            -- on the outer-most 'inExpr' will be common, and we should probably have a separate task for that.
                            location

                        DeclExpr _ _ ->
                            location

        PromoteLet ->
            case location of
                PatternLocation _ _ ->
                    location

                DeclLocation _ _ ->
                    location

                ExprLocation (Let _ _) _ ->
                    location

                ExprLocation expr path ->
                    case path of
                        LetExpr _ _ ->
                            location

                        _ ->
                            let
                                newDeclaration =
                                    { pattern = NamePattern "n"
                                    , expr = Leaf "3"
                                    }

                                newExpr =
                                    Let [ newDeclaration ] expr
                            in
                            ExprLocation newExpr path



-- In several places we need to combine a list of children which are in path form.
-- In this case, we'll have the left set of children, the 'hole' and the right set of
-- children, with the left set reversed. This function builds up the expected list of
-- children in the correct order.


upList : List a -> a -> List a -> List a
upList left hole right =
    List.reverse left ++ (hole :: right)


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
                    "Up"

                mMessage =
                    case location of
                        ExprLocation _ path ->
                            case path of
                                Top ->
                                    Nothing

                                ApplyPath _ _ _ ->
                                    action MoveUp

                                LetExpr _ _ ->
                                    action MoveUp

                                DeclExpr _ _ ->
                                    action MoveUp

                        PatternLocation _ path ->
                            case path of
                                DeclPattern _ _ ->
                                    action MoveUp

                        DeclLocation _ path ->
                            case path of
                                LetDecl _ _ _ _ ->
                                    action MoveUp
            in
            button title mMessage

        moveDown =
            let
                title =
                    "Down"

                mMessage =
                    case location of
                        ExprLocation expr _ ->
                            case expr of
                                Leaf _ ->
                                    Nothing

                                Apply _ ->
                                    action MoveDown

                                Let _ _ ->
                                    action MoveDown

                        PatternLocation pattern _ ->
                            case pattern of
                                NamePattern _ ->
                                    Nothing

                        DeclLocation _ _ ->
                            action MoveDown
            in
            button title mMessage

        moveLeft =
            let
                title =
                    "Left"

                mMessage =
                    case location of
                        ExprLocation _ path ->
                            case path of
                                Top ->
                                    Nothing

                                ApplyPath [] _ _ ->
                                    Nothing

                                ApplyPath (_ :: _) _ _ ->
                                    action MoveLeft

                                LetExpr [] _ ->
                                    Nothing

                                LetExpr (_ :: _) _ ->
                                    action MoveLeft

                                DeclExpr _ _ ->
                                    action MoveLeft

                        PatternLocation _ path ->
                            case path of
                                DeclPattern _ _ ->
                                    Nothing

                        DeclLocation _ path ->
                            case path of
                                LetDecl [] _ _ _ ->
                                    Nothing

                                LetDecl (_ :: _) _ _ _ ->
                                    action MoveLeft
            in
            button title mMessage

        moveRight =
            let
                title =
                    "Right"

                mMessage =
                    case location of
                        ExprLocation _ path ->
                            case path of
                                Top ->
                                    Nothing

                                ApplyPath _ _ [] ->
                                    Nothing

                                ApplyPath _ _ (_ :: _) ->
                                    action MoveRight

                                LetExpr _ _ ->
                                    Nothing

                                DeclExpr _ _ ->
                                    Nothing

                        PatternLocation _ path ->
                            case path of
                                DeclPattern _ _ ->
                                    action MoveRight

                        DeclLocation _ path ->
                            case path of
                                LetDecl _ _ _ _ ->
                                    action MoveRight
            in
            button title mMessage

        insertLeft =
            let
                title =
                    "InsertLeft"

                mMessage =
                    case location of
                        ExprLocation _ path ->
                            case path of
                                Top ->
                                    Nothing

                                ApplyPath _ _ _ ->
                                    action InsertLeft

                                LetExpr _ _ ->
                                    action InsertLeft

                                DeclExpr _ _ ->
                                    Nothing

                        PatternLocation _ path ->
                            case path of
                                DeclPattern _ _ ->
                                    Nothing

                        DeclLocation _ path ->
                            case path of
                                LetDecl _ _ _ _ ->
                                    action InsertLeft
            in
            button title mMessage

        insertRight =
            let
                title =
                    "InsertRight"

                mMessage =
                    case location of
                        ExprLocation _ path ->
                            case path of
                                Top ->
                                    Nothing

                                ApplyPath _ _ _ ->
                                    action InsertRight

                                LetExpr _ _ ->
                                    Nothing

                                DeclExpr _ _ ->
                                    Nothing

                        PatternLocation _ path ->
                            case path of
                                DeclPattern _ _ ->
                                    Nothing

                        DeclLocation _ path ->
                            case path of
                                LetDecl _ _ _ _ ->
                                    action InsertRight
            in
            button title mMessage

        promoteLet =
            let
                title =
                    "Let"

                mMessage =
                    case location of
                        ExprLocation expr path ->
                            case path of
                                LetExpr _ _ ->
                                    Nothing

                                _ ->
                                    case expr of
                                        Let _ _ ->
                                            Nothing

                                        Leaf _ ->
                                            action PromoteLet

                                        Apply _ ->
                                            action PromoteLet

                        PatternLocation _ _ ->
                            Nothing

                        DeclLocation _ _ ->
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


viewExpr : Expr -> Element msg
viewExpr expr =
    case expr of
        Leaf s ->
            text s

        Apply children ->
            viewApply <| List.map viewExpr children

        Let declarations inExpr ->
            viewLet (viewDeclarationList declarations) (viewExpr inExpr)


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


viewDeclaration : Element msg -> Element msg -> Element msg
viewDeclaration declPattern declExpr =
    Element.column
        [ Element.padding 10 ]
        [ Element.row
            [ Element.spacing 8 ]
            [ el
                [ Element.width Element.fill
                , Font.color themeColor4
                ]
                declPattern
            , viewPunctuation "="
            ]
        , el
            [ indentElement ]
            declExpr
        ]


viewDeclarationList : List Declaration -> List (Element msg)
viewDeclarationList declarations =
    let
        viewDecl decl =
            viewDeclaration
                (viewPattern decl.pattern)
                (viewExpr decl.expr)
    in
    List.map viewDecl declarations


viewPattern : Pattern -> Element msg
viewPattern pattern =
    case pattern of
        NamePattern s ->
            text s


viewApply : List (Element msg) -> Element msg
viewApply =
    Element.wrappedRow
        [ Element.spacing 4 ]



-- An alternative and easier way to do this, would be to just go-up until you reach the root
-- and then draw that. It would mean creating the entire source tree, but what you're doing
-- currently is a way of just cutting out that intermediate data structure, but the one you're
-- building will be the same order. So you won't save much unless the tree is large.
-- Another option, is to memoise the view of a tree, we can probably do this somehow with Element.lazy.


viewExprPath : Element msg -> ExprPath -> Element msg
viewExprPath viewed path =
    case path of
        Top ->
            el
                [ classAttribute "top" ]
                viewed

        ApplyPath left up right ->
            let
                leftViewed =
                    List.map viewExpr left

                rightViewed =
                    List.map viewExpr right

                children =
                    upList leftViewed viewed rightViewed

                child =
                    viewApply children
            in
            viewExprPath child up

        LetExpr declarations up ->
            let
                child =
                    viewLet (List.reverse <| viewDeclarationList declarations) viewed
            in
            viewExprPath child up

        DeclExpr pattern up ->
            let
                child =
                    viewDeclaration (viewPattern pattern) viewed
            in
            viewDeclPath child up


viewDeclPath : Element msg -> DeclPath -> Element msg
viewDeclPath viewed path =
    case path of
        LetDecl left up right expr ->
            let
                leftViewed =
                    viewDeclarationList left

                rightViewed =
                    viewDeclarationList right

                declarations =
                    upList leftViewed viewed rightViewed

                child =
                    viewLet declarations <| viewExpr expr
            in
            viewExprPath child up


viewPatternPath : Element msg -> PatternPath -> Element msg
viewPatternPath viewed path =
    case path of
        DeclPattern up expr ->
            let
                child =
                    viewDeclaration viewed <| viewExpr expr
            in
            viewDeclPath child up


viewHighlighted : Element Msg -> Element Msg
viewHighlighted viewed =
    el
        [ Border.width 2
        , Border.rounded 5
        , Border.color themeColor2
        , Element.padding 10
        ]
        viewed


viewFocusedLeaf : String -> Element Msg
viewFocusedLeaf contents =
    Input.text
        [ classAttribute "leaf-input"
        , idAttribute leafBoxId
        , Element.width Element.shrink
        ]
        { onChange = LeafInput
        , text = contents
        , placeholder = Nothing
        , label = Input.labelHidden ""
        }


viewLocation : Location -> Element Msg
viewLocation location =
    case location of
        ExprLocation expr path ->
            let
                viewed =
                    case expr of
                        Leaf s ->
                            viewFocusedLeaf s

                        _ ->
                            viewHighlighted <| viewExpr expr
            in
            viewExprPath viewed path

        DeclLocation declaration path ->
            let
                viewed =
                    viewHighlighted <|
                        viewDeclaration
                            (viewPattern declaration.pattern)
                            (viewExpr declaration.expr)
            in
            viewDeclPath viewed path

        PatternLocation pattern path ->
            let
                viewed =
                    case pattern of
                        NamePattern s ->
                            viewFocusedLeaf s
            in
            viewPatternPath viewed path


idAttribute : String -> Element.Attribute msg
idAttribute s =
    Element.htmlAttribute <| Attributes.id s


classAttribute : String -> Element.Attribute msg
classAttribute s =
    Element.htmlAttribute <| Attributes.class s
