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

        keyToEditorAction : String -> Maybe Action
        keyToEditorAction string =
            case string of
                "ArrowLeft" ->
                    Just goLeft

                "ArrowRight" ->
                    Just goRight

                "ArrowUp" ->
                    Just goUp

                "ArrowDown" ->
                    Just goDown

                _ ->
                    Nothing
    in
    Browser.Events.onKeyPress keyDecoder


type alias ProgramFlags =
    ()


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , editorState : EditorState
    }


type alias EditorState =
    { location : Location
    , clipBoard : Maybe Fragment
    }



-- A fragment is *really* just the 'hole' part of the location, so in other words
-- we never need to worry about the path contained within a fragment. It is
-- therefore tempting to make a separate type. We'll see how this plays out
-- but if we ever find ourselves with a bug because of this we should just make
-- type Fragment = ExprFragment Expr | ...


type alias Fragment =
    Location


type Expr
    = Leaf String
    | Apply (List Expr)
    | Let (List ValueDeclaration) Expr


type alias ValueDeclaration =
    { pattern : Pattern
    , expr : Expr
    }


type Pattern
    = NamePattern String


type ExprPath
    = ApplyPath (List Expr) ExprPath (List Expr)
    | LetExpr (List ValueDeclaration) ExprPath
    | DeclExpr Pattern DeclPath
    | ModuleDeclExpr Pattern ModuleDeclPath


type DeclPath
    = LetDecl (List ValueDeclaration) ExprPath (List ValueDeclaration) Expr


type ModuleDeclPath
    = ModuleDecl (List ModuleDeclaration) (List ModuleDeclaration)


type PatternPath
    = DeclPattern DeclPath Expr
    | ModuleDeclPattern ModuleDeclPath Expr


type ModuleDeclaration
    = ModuleValueDeclaration ValueDeclaration
    | ModuleTypeDeclaration TypeDeclaration


type alias TypeDeclaration =
    { pattern : TypePattern
    , typeExpr : TypeExpr
    }


type TypePattern
    = NameTypePattern String


type TypePatternPath
    = ModuleDeclTypePattern ModuleDeclPath TypeExpr


type TypeExpr
    = NameType String


type TypeExprPath
    = ModuleDeclType TypePattern ModuleDeclPath


type alias Module =
    List ModuleDeclaration


type Location
    = ExprLocation Expr ExprPath
    | DeclLocation ValueDeclaration DeclPath
    | PatternLocation Pattern PatternPath
    | TypeExprLocation TypeExpr TypeExprPath
    | TypePatternLocation TypePattern TypePatternPath
    | ModuleDeclLocation ModuleDeclaration ModuleDeclPath
    | ModuleLocation Module


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

        moduleDecl =
            ModuleValueDeclaration { pattern = NamePattern "main", expr = expr }

        typeDecl =
            ModuleTypeDeclaration { pattern = NameTypePattern "Person", typeExpr = NameType "String" }
    in
    ModuleLocation [ typeDecl, moduleDecl ]


init : ProgramFlags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        initialModel =
            { key = key
            , url = url
            , editorState = initialState
            }

        initialState =
            { location = initialLocation
            , clipBoard = Nothing
            }

        initialCommand =
            Cmd.none
    in
    ( initialModel, initialCommand )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | EditorAction Action
    | LeafInput String
    | NoOp


type alias Action =
    { updateState : EditorState -> EditorState
    , isAvailable : EditorState -> Bool
    }



-- The default function for checking whether an action is available or not is just to apply the
-- given update location function and see if the location has changed. Now this obviously may result
-- in a fair amount of unnecessary work for certain actions, in which case we can simply provide a more
-- sophisticated `isAvailable` function.


defaultIsAvailable : (EditorState -> EditorState) -> EditorState -> Bool
defaultIsAvailable updateState state =
    state /= updateState state


updateStateLocation : (Location -> Location) -> EditorState -> EditorState
updateStateLocation f state =
    { state | location = f state.location }


goLeft : Action
goLeft =
    let
        updateLocation location =
            case location of
                ModuleLocation _ ->
                    location

                ExprLocation expr path ->
                    case path of
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

                        ModuleDeclExpr pattern up ->
                            PatternLocation pattern <| ModuleDeclPattern up expr

                DeclLocation declaration path ->
                    case path of
                        LetDecl [] _ _ _ ->
                            location

                        LetDecl (l :: left) up right expr ->
                            DeclLocation l <| LetDecl left up (declaration :: right) expr

                PatternLocation pattern path ->
                    case path of
                        DeclPattern _ _ ->
                            location

                        ModuleDeclPattern _ _ ->
                            location

                TypeExprLocation typeExpr path ->
                    case path of
                        ModuleDeclType tPattern mDeclPath ->
                            TypePatternLocation tPattern <|
                                ModuleDeclTypePattern mDeclPath typeExpr

                TypePatternLocation _ path ->
                    case path of
                        ModuleDeclTypePattern _ _ ->
                            location

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl [] _ ->
                            location

                        ModuleDecl (l :: left) right ->
                            ModuleDeclLocation l <| ModuleDecl left (mDecl :: right)

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


goRight : Action
goRight =
    let
        updateLocation location =
            case location of
                ModuleLocation _ ->
                    location

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl _ [] ->
                            location

                        ModuleDecl left (r :: right) ->
                            ModuleDeclLocation r <| ModuleDecl (mDecl :: left) right

                TypeExprLocation typeExpr path ->
                    case path of
                        ModuleDeclType tPattern mDeclPath ->
                            location

                TypePatternLocation tPattern path ->
                    case path of
                        ModuleDeclTypePattern up typeExpr ->
                            TypeExprLocation typeExpr <| ModuleDeclType tPattern up

                PatternLocation pattern path ->
                    case path of
                        DeclPattern up expr ->
                            ExprLocation expr <| DeclExpr pattern up

                        ModuleDeclPattern up expr ->
                            ExprLocation expr <| ModuleDeclExpr pattern up

                DeclLocation declaration path ->
                    case path of
                        LetDecl left up [] expr ->
                            ExprLocation expr <| LetExpr (declaration :: left) up

                        LetDecl left up (r :: right) expr ->
                            DeclLocation r <| LetDecl (declaration :: left) up right expr

                ExprLocation expr path ->
                    case path of
                        ApplyPath _ _ [] ->
                            location

                        ApplyPath left up (r :: right) ->
                            ExprLocation r <| ApplyPath (expr :: left) up right

                        LetExpr _ _ ->
                            location

                        DeclExpr _ _ ->
                            location

                        ModuleDeclExpr _ _ ->
                            location

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


goUp : Action
goUp =
    let
        updateLocation location =
            case location of
                ModuleLocation _ ->
                    location

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl left right ->
                            let
                                moduleTerm =
                                    upList left mDecl right
                            in
                            ModuleLocation moduleTerm

                TypeExprLocation typeExpr path ->
                    case path of
                        ModuleDeclType tPattern up ->
                            let
                                declaration =
                                    ModuleTypeDeclaration { pattern = tPattern, typeExpr = typeExpr }
                            in
                            ModuleDeclLocation declaration up

                TypePatternLocation tPattern path ->
                    case path of
                        ModuleDeclTypePattern up typeExpr ->
                            let
                                declaration =
                                    ModuleTypeDeclaration { pattern = tPattern, typeExpr = typeExpr }
                            in
                            ModuleDeclLocation declaration up

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

                        ModuleDeclPattern up expr ->
                            let
                                declaration =
                                    { pattern = pattern
                                    , expr = expr
                                    }

                                mDecl =
                                    ModuleValueDeclaration declaration
                            in
                            ModuleDeclLocation mDecl up

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

                        ModuleDeclExpr pattern up ->
                            let
                                declaration =
                                    { pattern = pattern
                                    , expr = expr
                                    }

                                mDecl =
                                    ModuleValueDeclaration declaration
                            in
                            ModuleDeclLocation mDecl up

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


goDown : Action
goDown =
    let
        updateLocation location =
            case location of
                ModuleLocation [] ->
                    location

                ModuleLocation (first :: rest) ->
                    ModuleDeclLocation first <| ModuleDecl [] rest

                ModuleDeclLocation mDecl up ->
                    case mDecl of
                        ModuleValueDeclaration declaration ->
                            PatternLocation declaration.pattern <|
                                ModuleDeclPattern up declaration.expr

                        ModuleTypeDeclaration declaration ->
                            TypePatternLocation declaration.pattern <|
                                ModuleDeclTypePattern up declaration.typeExpr

                TypePatternLocation tPattern path ->
                    case tPattern of
                        NameTypePattern _ ->
                            location

                TypeExprLocation typeExpr path ->
                    case typeExpr of
                        NameType _ ->
                            location

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
                            -- have GoDownLeft and GoDownRight.
                            ExprLocation inExpr <| LetExpr [] up

                        Let (first :: rest) inExpr ->
                            DeclLocation first <| LetDecl [] up rest inExpr

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


insertLeft : Action
insertLeft =
    let
        updateLocation location =
            case location of
                ModuleLocation _ ->
                    -- This is technically correct, but it's tempting to allow the insertion of
                    -- some import statement here.
                    location

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl left right ->
                            let
                                declaration =
                                    { pattern = NamePattern "ml"
                                    , expr = Leaf "1"
                                    }

                                newMDecl =
                                    ModuleValueDeclaration declaration
                            in
                            ModuleDeclLocation newMDecl <|
                                ModuleDecl left (mDecl :: right)

                TypePatternLocation tPattern path ->
                    location

                TypeExprLocation typeExpr path ->
                    location

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

                        ModuleDeclExpr _ _ ->
                            location

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


insertRight : Action
insertRight =
    let
        updateLocation location =
            case location of
                ModuleLocation _ ->
                    -- This is technically correct, but it's tempting to allow the insertion of
                    -- some value declaration to the end of the module file.
                    location

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl left right ->
                            let
                                declaration =
                                    { pattern = NamePattern "mr"
                                    , expr = Leaf "1"
                                    }

                                newMDecl =
                                    ModuleValueDeclaration declaration
                            in
                            ModuleDeclLocation newMDecl <|
                                ModuleDecl (mDecl :: left) right

                TypePatternLocation tPattern path ->
                    location

                TypeExprLocation typeExpr path ->
                    location

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
                        ApplyPath left up right ->
                            let
                                newExpr =
                                    Leaf "e"
                            in
                            ExprLocation newExpr <|
                                ApplyPath (expr :: left) up right

                        LetExpr _ _ ->
                            -- In theory we could interpret this as 'insert a new declaration at the bottom of the list of declarations'
                            -- I suspect this is something of a common task, it's easy enough from here anyway you simply go Right->insertRight.
                            -- In general I suspect adding a new declaration to the inner most enclosing 'let' even if you're not actually
                            -- on the outer-most 'inExpr' will be common, and we should probably have a separate task for that.
                            location

                        DeclExpr _ _ ->
                            location

                        ModuleDeclExpr _ _ ->
                            location

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


moveLeft : Action
moveLeft =
    let
        updateLocation location =
            case location of
                ModuleLocation _ ->
                    location

                ExprLocation expr path ->
                    case path of
                        ApplyPath [] _ _ ->
                            location

                        ApplyPath (l :: left) up right ->
                            ExprLocation expr <|
                                ApplyPath left up (l :: right)

                        LetExpr [] _ ->
                            location

                        LetExpr (l :: left) up ->
                            location

                        DeclExpr pattern up ->
                            location

                        ModuleDeclExpr pattern up ->
                            location

                DeclLocation declaration path ->
                    case path of
                        LetDecl [] _ _ _ ->
                            location

                        LetDecl (l :: left) up right expr ->
                            DeclLocation declaration <|
                                LetDecl left up (l :: right) expr

                PatternLocation pattern path ->
                    case path of
                        DeclPattern _ _ ->
                            location

                        ModuleDeclPattern _ _ ->
                            location

                TypeExprLocation typeExpr path ->
                    case path of
                        ModuleDeclType tPattern mDeclPath ->
                            location

                TypePatternLocation _ path ->
                    case path of
                        ModuleDeclTypePattern _ _ ->
                            location

                ModuleDeclLocation moduleDecl path ->
                    case path of
                        ModuleDecl [] _ ->
                            location

                        ModuleDecl (l :: left) right ->
                            ModuleDeclLocation moduleDecl <| ModuleDecl left (l :: right)

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


moveRight : Action
moveRight =
    let
        updateLocation location =
            case location of
                ModuleLocation _ ->
                    location

                ModuleDeclLocation moduleDecl path ->
                    case path of
                        ModuleDecl _ [] ->
                            location

                        ModuleDecl left (r :: right) ->
                            ModuleDeclLocation moduleDecl <| ModuleDecl (r :: left) right

                TypeExprLocation typeExpr path ->
                    case path of
                        ModuleDeclType tPattern mDeclPath ->
                            location

                TypePatternLocation tPattern path ->
                    case path of
                        ModuleDeclTypePattern up typeExpr ->
                            location

                PatternLocation pattern path ->
                    case path of
                        DeclPattern up expr ->
                            location

                        ModuleDeclPattern up expr ->
                            location

                DeclLocation declaration path ->
                    case path of
                        LetDecl left up [] expr ->
                            location

                        LetDecl left up (r :: right) expr ->
                            DeclLocation declaration <| LetDecl (r :: left) up right expr

                ExprLocation expr path ->
                    case path of
                        ApplyPath _ _ [] ->
                            location

                        ApplyPath left up (r :: right) ->
                            ExprLocation expr <| ApplyPath (r :: left) up right

                        LetExpr _ _ ->
                            location

                        DeclExpr _ _ ->
                            location

                        ModuleDeclExpr _ _ ->
                            location

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


promoteLet : Action
promoteLet =
    let
        updateLocation location =
            case location of
                ModuleLocation _ ->
                    location

                ModuleDeclLocation _ _ ->
                    location

                TypePatternLocation tPattern path ->
                    location

                TypeExprLocation typeExpr path ->
                    location

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

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


insertTypeDecl : Action
insertTypeDecl =
    let
        updateLocation location =
            case location of
                ModuleLocation moduleDecls ->
                    -- TODO: We would be better to just go down first, and then insert the type declaration,
                    -- rather than have this kind of duplication of code here.
                    let
                        newTypeDecl =
                            ModuleTypeDeclaration { pattern = NameTypePattern "A", typeExpr = NameType "String" }
                    in
                    ModuleDeclLocation newTypeDecl <|
                        ModuleDecl [] moduleDecls

                ModuleDeclLocation currentDecl path ->
                    case path of
                        ModuleDecl left right ->
                            let
                                newTypeDecl =
                                    ModuleTypeDeclaration { pattern = NameTypePattern "A", typeExpr = NameType "String" }
                            in
                            ModuleDeclLocation newTypeDecl <|
                                ModuleDecl left (currentDecl :: right)

                -- All of these remaining ones, could absolutely accept a InsertTypeDecl, you just need to go up
                -- until you get to the module declarations and *then* insert the new type declaration.
                TypePatternLocation tPattern path ->
                    location

                TypeExprLocation typeExpr path ->
                    location

                PatternLocation _ _ ->
                    location

                DeclLocation _ _ ->
                    location

                ExprLocation (Let _ _) _ ->
                    location

                ExprLocation expr path ->
                    location

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


cutAction : Action
cutAction =
    let
        updateState editorState =
            case editorState.location of
                ExprLocation expr path ->
                    let
                        buildPath left up right =
                            case List.isEmpty left && List.isEmpty right of
                                True ->
                                    up

                                False ->
                                    ApplyPath left up right
                    in
                    case path of
                        ApplyPath [] up [] ->
                            -- This is an impossible state that we shouldn't get into, we could do some tricks
                            -- for example we could update the location to `ExprLocation expr up` and then recursively
                            -- call `updateState. That would introduce the possiblity of an infinite loop. We could
                            -- alternatively fix up the current location, put that on the clipboard and carry on, so this
                            -- cut action would act like a copy one, but also fix up the impossible state. That's nice, but
                            -- let's just fail in an obvious way so that hopefully the underlying bug gets fixed.
                            editorState

                        ApplyPath (l :: left) up right ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation l <| buildPath left up right
                            }

                        ApplyPath [] up (r :: right) ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation r <| buildPath [] up right
                            }

                        LetExpr _ _ ->
                            -- You cannot cut the expression from a let.
                            editorState

                        DeclExpr _ _ ->
                            -- You cannot cut the expression from a declaration.
                            editorState

                        ModuleDeclExpr _ _ ->
                            -- You cannot cut the expression from a module declaration
                            editorState

                DeclLocation valueDecl path ->
                    case path of
                        LetDecl [] up [] expr ->
                            -- Here you're cutting the very last declaration of a let, that's fine
                            -- but it is now no longer a let expression.
                            { clipBoard = Just editorState.location
                            , location = ExprLocation expr up
                            }

                        LetDecl (l :: left) up right expr ->
                            { clipBoard = Just editorState.location
                            , location =
                                DeclLocation l <|
                                    LetDecl left up right expr
                            }

                        LetDecl [] up (r :: right) expr ->
                            { clipBoard = Just editorState.location
                            , location =
                                DeclLocation r <|
                                    LetDecl [] up right expr
                            }

                PatternLocation pattern path ->
                    case path of
                        DeclPattern _ _ ->
                            editorState

                        ModuleDeclPattern _ _ ->
                            editorState

                TypeExprLocation typeExpr path ->
                    case path of
                        ModuleDeclType _ _ ->
                            editorState

                TypePatternLocation typePattern path ->
                    case path of
                        ModuleDeclTypePattern _ _ ->
                            editorState

                ModuleDeclLocation moduleDecl path ->
                    case path of
                        ModuleDecl [] [] ->
                            { clipBoard = Just editorState.location
                            , location = ModuleLocation []
                            }

                        ModuleDecl (l :: left) right ->
                            { clipBoard = Just editorState.location
                            , location =
                                ModuleDeclLocation l <|
                                    ModuleDecl left right
                            }

                        ModuleDecl [] (r :: right) ->
                            { clipBoard = Just editorState.location
                            , location =
                                ModuleDeclLocation r <|
                                    ModuleDecl [] right
                            }

                ModuleLocation _ ->
                    editorState
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


copyAction : Action
copyAction =
    let
        updateState editorState =
            { editorState | clipBoard = Just editorState.location }
    in
    { updateState = updateState

    -- We could just make this `always True` as it is always possible to perform
    -- a copy. However, it is also very cheap to perform a copy, and the one time
    -- we want to disable it, is if the current clipboard is equal to the current
    -- location, since copying will have no effect, and this will be disabled.
    -- However, we may find that users find this counter-intuitive, and wonder why
    -- 'copy' is disabled. In other words  there may be less cognitive load in simply
    -- performing the useless operation than in having them wonder why it is disabled.
    , isAvailable = defaultIsAvailable updateState
    }


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

        EditorAction editorAction ->
            let
                newModel =
                    { model | editorState = editorAction.updateState model.editorState }

                command =
                    case newModel.editorState.location of
                        ExprLocation (Leaf _) _ ->
                            focusLeafBox

                        PatternLocation (NamePattern _) _ ->
                            focusLeafBox

                        TypePatternLocation (NameTypePattern _) _ ->
                            focusLeafBox

                        TypeExprLocation (NameType _) _ ->
                            focusLeafBox

                        _ ->
                            Cmd.none
            in
            ( newModel, command )

        LeafInput content ->
            let
                updateLocation l =
                    let
                        editorState =
                            model.editorState

                        newState =
                            { editorState | location = l }
                    in
                    { model | editorState = newState }

                newModel =
                    case model.editorState.location of
                        ExprLocation (Leaf _) path ->
                            updateLocation <|
                                ExprLocation (Leaf content) path

                        PatternLocation (NamePattern _) path ->
                            updateLocation <|
                                PatternLocation (NamePattern content) path

                        TypeExprLocation (NameType _) path ->
                            updateLocation <|
                                TypeExprLocation (NameType content) path

                        TypePatternLocation (NameTypePattern _) path ->
                            updateLocation <|
                                TypePatternLocation (NameTypePattern content) path

                        ExprLocation _ _ ->
                            model

                        DeclLocation _ _ ->
                            model

                        ModuleDeclLocation _ _ ->
                            model

                        ModuleLocation _ ->
                            model
            in
            withCommands newModel []


withCommands : model -> List (Cmd msg) -> ( model, Cmd msg )
withCommands model commands =
    ( model, Cmd.batch commands )



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
                [ header model.editorState
                , viewLocation model.editorState.location
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


header : EditorState -> Element Msg
header editorState =
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

        makeButton editorAction title =
            case editorAction.isAvailable editorState of
                True ->
                    button title <| Just (EditorAction editorAction)

                False ->
                    button title Nothing
    in
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spaceEvenly
        ]
        [ makeButton goUp "Up"
        , makeButton goDown "Down"
        , makeButton goLeft "Left"
        , makeButton goRight "Right"
        , makeButton insertLeft "InsertLeft"
        , makeButton insertRight "InsertRight"
        , makeButton moveLeft "MoveLeft"
        , makeButton moveRight "MoveRight"
        , makeButton insertTypeDecl "Declare type"
        , makeButton promoteLet "Let"
        , makeButton cutAction "Cut"
        , makeButton copyAction "Copy"
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


viewDeclarationList : List ValueDeclaration -> List (Element msg)
viewDeclarationList declarations =
    List.map viewDecl declarations



-- We should use a scheme to distinguish between laying out a tree segment from the
-- already viewed constitient parts, and viewing something from scratch, eg.
-- viewDeclaration : Declaration -> Element msg
-- which would use
-- layoutDeclaration : Element msg -> Element msg -> Element msg


viewDecl : ValueDeclaration -> Element msg
viewDecl decl =
    viewDeclaration
        (viewPattern decl.pattern)
        (viewExpr decl.expr)


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

        ModuleDeclExpr pattern up ->
            let
                child =
                    viewDeclaration (viewPattern pattern) viewed
            in
            viewModuleDeclPath child up


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

        ModuleDeclPattern up expr ->
            let
                child =
                    viewDeclaration viewed <| viewExpr expr
            in
            viewModuleDeclPath child up


viewTypePattern : TypePattern -> Element msg
viewTypePattern typePattern =
    case typePattern of
        NameTypePattern s ->
            text s


viewTypeExpr : TypeExpr -> Element msg
viewTypeExpr typeExpr =
    case typeExpr of
        NameType s ->
            text s


layoutTypeDeclaration : Element msg -> Element msg -> Element msg
layoutTypeDeclaration viewedTypePattern viewedTypeExpr =
    Element.column
        []
        [ Element.row
            [ Element.spacing 5 ]
            [ viewKeyword "type"
            , viewedTypePattern
            , viewPunctuation "="
            ]
        , viewedTypeExpr
        ]


viewTypeExprPath : Element msg -> TypeExprPath -> Element msg
viewTypeExprPath viewed path =
    case path of
        ModuleDeclType tPattern up ->
            let
                newViewed =
                    layoutTypeDeclaration
                        (viewTypePattern tPattern)
                        viewed
            in
            viewModuleDeclPath newViewed up


viewTypePatternPath : Element msg -> TypePatternPath -> Element msg
viewTypePatternPath viewed path =
    case path of
        ModuleDeclTypePattern up tExpr ->
            let
                newViewed =
                    layoutTypeDeclaration
                        viewed
                        (viewTypeExpr tExpr)
            in
            viewModuleDeclPath newViewed up


viewModuleDeclaration : ModuleDeclaration -> Element msg
viewModuleDeclaration mDecl =
    case mDecl of
        ModuleValueDeclaration declaration ->
            viewDecl declaration

        ModuleTypeDeclaration declaration ->
            layoutTypeDeclaration
                (viewTypePattern declaration.pattern)
                (viewTypeExpr declaration.typeExpr)


viewModuleDeclPath : Element msg -> ModuleDeclPath -> Element msg
viewModuleDeclPath viewed path =
    case path of
        ModuleDecl left right ->
            let
                leftViewed =
                    List.map viewModuleDeclaration left

                rightViewed =
                    List.map viewModuleDeclaration right
            in
            viewModule <| upList leftViewed viewed rightViewed


viewModule : List (Element msg) -> Element msg
viewModule mDecls =
    let
        heading =
            Element.row
                [ Element.spacing 2 ]
                [ viewKeyword "module"
                , text "Main"
                , viewKeyword "exposing"
                , viewPunctuation "(..)"
                ]
    in
    Element.column
        [ Element.spacing 10 ]
        (heading :: mDecls)


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
        ModuleLocation mDecls ->
            viewHighlighted <| viewModule <| List.map viewModuleDeclaration mDecls

        ModuleDeclLocation mDecl path ->
            let
                viewed =
                    viewHighlighted <| viewModuleDeclaration mDecl
            in
            viewModuleDeclPath viewed path

        TypeExprLocation tExpr path ->
            let
                viewed =
                    case tExpr of
                        NameType s ->
                            viewFocusedLeaf s
            in
            viewTypeExprPath viewed path

        TypePatternLocation tPattern path ->
            let
                viewed =
                    case tPattern of
                        NameTypePattern s ->
                            viewFocusedLeaf s
            in
            viewTypePatternPath viewed path

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
