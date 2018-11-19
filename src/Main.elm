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
    | ApplyPattern (List Pattern)


type ExprPath
    = ApplyPath (List Expr) ExprPath (List Expr)
    | LetExpr (List ValueDeclaration) ExprPath
    | DeclExpr Pattern DeclPath
    | ModuleDeclExpr Pattern ModuleDeclPath


type DeclPath
    = LetDecl (List ValueDeclaration) ExprPath (List ValueDeclaration) Expr


type ModuleDeclPath
    = ModuleDecl ModuleName (List Export) (List Import) (List ModuleDeclaration) (List ModuleDeclaration)


type ModuleImportPath
    = ModuleImportPath ModuleName (List Export) (List Import) (List Import) (List ModuleDeclaration)


type ModuleExportPath
    = ModuleExportPath ModuleName (List Export) (List Export) (List Import) (List ModuleDeclaration)


type ModuleNamePath
    = ModuleNamePath (List Export) (List Import) (List ModuleDeclaration)


type PatternPath
    = ApplyPatternPath (List Pattern) PatternPath (List Pattern)
    | DeclPattern DeclPath Expr
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


type alias Export =
    { name : String
    , exportConstructors : Bool
    }



-- TODO: Obviously should be more complicated than this, in particular can have an 'as' clause
-- and can have an exposing clause.


type alias Import =
    String


type alias ModuleName =
    String


type alias Module =
    { name : ModuleName
    , exports : List Export
    , imports : List Import
    , declarations : List ModuleDeclaration
    }


type Location
    = ExprLocation Expr ExprPath
    | DeclLocation ValueDeclaration DeclPath
    | PatternLocation Pattern PatternPath
    | TypeExprLocation TypeExpr TypeExprPath
    | TypePatternLocation TypePattern TypePatternPath
    | ModuleImportLocation Import ModuleImportPath
    | ModuleExportLocation Export ModuleExportPath
    | ModuleNameLocation ModuleName ModuleNamePath
    | ModuleDeclLocation ModuleDeclaration ModuleDeclPath
    | ModuleLocation Module


initialLocation : Location
initialLocation =
    let
        expr =
            Let
                [ { pattern = ApplyPattern [ NamePattern "Just", NamePattern "a" ], expr = Apply [ Leaf "Just", Leaf "1" ] }
                , { pattern = NamePattern "b", expr = Leaf "2" }
                , { pattern = NamePattern "c", expr = Leaf "3" }
                , { pattern = NamePattern "d", expr = Leaf "4" }
                ]
                (Apply [ Leaf "a", Leaf "b", Leaf "c", Leaf "d" ])

        moduleDecl =
            ModuleValueDeclaration { pattern = NamePattern "main", expr = expr }

        typeDecl =
            ModuleTypeDeclaration { pattern = NameTypePattern "Person", typeExpr = NameType "String" }

        moduleTerm =
            { name = "Main"
            , exports = [ { name = "main", exportConstructors = False } ]
            , imports = [ "Html", "Html.Attributes", "Html.Events" ]
            , declarations = [ typeDecl, moduleDecl ]
            }
    in
    ModuleLocation moduleTerm


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

                ModuleImportLocation importTerm path ->
                    case path of
                        ModuleImportPath moduleName [] [] right declarations ->
                            ModuleNameLocation moduleName <|
                                ModuleNamePath [] (importTerm :: right) declarations

                        ModuleImportPath moduleName (e :: exports) [] right declarations ->
                            ModuleExportLocation e <|
                                ModuleExportPath moduleName exports [] (importTerm :: right) declarations

                        ModuleImportPath moduleName exports (l :: left) right declarations ->
                            ModuleImportLocation l <|
                                ModuleImportPath moduleName exports left (importTerm :: right) declarations

                ModuleExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName [] right imports declarations ->
                            ModuleNameLocation moduleName <|
                                ModuleNamePath (export :: right) imports declarations

                        ModuleExportPath moduleName (l :: left) right imports declarations ->
                            ModuleExportLocation l <|
                                ModuleExportPath moduleName left (export :: right) imports declarations

                ModuleNameLocation _ path ->
                    case path of
                        ModuleNamePath _ _ _ ->
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
                        ApplyPatternPath [] _ _ ->
                            location

                        ApplyPatternPath (l :: left) up right ->
                            PatternLocation l <| ApplyPatternPath left up (pattern :: right)

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
                        ModuleDecl moduleName [] [] [] right ->
                            ModuleNameLocation moduleName <| ModuleNamePath [] [] (mDecl :: right)

                        ModuleDecl moduleName (e :: exports) [] [] right ->
                            ModuleExportLocation e <| ModuleExportPath moduleName exports [] [] (mDecl :: right)

                        ModuleDecl moduleName exports (i :: imports) [] right ->
                            ModuleImportLocation i <| ModuleImportPath moduleName exports imports [] (mDecl :: right)

                        ModuleDecl moduleName exports imports (l :: left) right ->
                            ModuleDeclLocation l <| ModuleDecl moduleName exports imports left (mDecl :: right)

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

                ModuleImportLocation importTerm path ->
                    case path of
                        ModuleImportPath moduleName exports left [] [] ->
                            location

                        ModuleImportPath moduleName exports left [] (decl :: declarations) ->
                            ModuleDeclLocation decl <|
                                ModuleDecl moduleName exports (importTerm :: left) [] declarations

                        ModuleImportPath moduleName exports left (r :: right) declarations ->
                            ModuleImportLocation r <|
                                ModuleImportPath moduleName exports (importTerm :: left) right declarations

                ModuleExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName left [] [] [] ->
                            location

                        ModuleExportPath moduleName left [] [] (decl :: declarations) ->
                            ModuleDeclLocation decl <|
                                ModuleDecl moduleName (export :: left) [] [] declarations

                        ModuleExportPath moduleName left [] (imp :: imports) declarations ->
                            ModuleImportLocation imp <|
                                ModuleImportPath moduleName (export :: left) [] imports declarations

                        ModuleExportPath moduleName left (r :: right) imports declarations ->
                            ModuleExportLocation r <|
                                ModuleExportPath moduleName (export :: left) right imports declarations

                ModuleNameLocation moduleName path ->
                    case path of
                        ModuleNamePath [] [] [] ->
                            location

                        ModuleNamePath [] [] (decl :: declarations) ->
                            ModuleDeclLocation decl <|
                                ModuleDecl moduleName [] [] [] declarations

                        ModuleNamePath [] (imp :: imports) declarations ->
                            ModuleImportLocation imp <|
                                ModuleImportPath moduleName [] [] imports declarations

                        ModuleNamePath (export :: exports) imports declarations ->
                            ModuleExportLocation export <|
                                ModuleExportPath moduleName [] exports imports declarations

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl _ _ _ _ [] ->
                            location

                        ModuleDecl moduleName exports imports left (r :: right) ->
                            ModuleDeclLocation r <| ModuleDecl moduleName exports imports (mDecl :: left) right

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
                        ApplyPatternPath _ _ [] ->
                            location

                        ApplyPatternPath left up (r :: right) ->
                            PatternLocation r <| ApplyPatternPath (pattern :: left) up right

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

                ModuleImportLocation importTerm path ->
                    case path of
                        ModuleImportPath moduleName exports left right declarations ->
                            let
                                moduleTerm =
                                    { name = moduleName
                                    , exports = exports
                                    , imports = upList left importTerm right
                                    , declarations = declarations
                                    }
                            in
                            ModuleLocation moduleTerm

                ModuleExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName left right imports declarations ->
                            let
                                moduleTerm =
                                    { name = moduleName
                                    , exports = upList left export right
                                    , imports = imports
                                    , declarations = declarations
                                    }
                            in
                            ModuleLocation moduleTerm

                ModuleNameLocation moduleName path ->
                    case path of
                        ModuleNamePath exports imports declarations ->
                            let
                                moduleTerm =
                                    { name = moduleName
                                    , exports = exports
                                    , imports = imports
                                    , declarations = declarations
                                    }
                            in
                            ModuleLocation moduleTerm

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl moduleName exports imports left right ->
                            let
                                moduleTerm =
                                    { name = moduleName
                                    , exports = exports
                                    , imports = imports
                                    , declarations = upList left mDecl right
                                    }
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
                        ApplyPatternPath left up right ->
                            let
                                newPattern =
                                    ApplyPattern <| upList left pattern right
                            in
                            PatternLocation newPattern up

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
                ModuleLocation moduleTerm ->
                    case moduleTerm.declarations of
                        first :: rest ->
                            ModuleDeclLocation first <|
                                ModuleDecl moduleTerm.name moduleTerm.exports moduleTerm.imports [] rest

                        [] ->
                            case moduleTerm.imports of
                                importTerm :: imports ->
                                    ModuleImportLocation importTerm <|
                                        ModuleImportPath moduleTerm.name moduleTerm.exports [] imports moduleTerm.declarations

                                [] ->
                                    case moduleTerm.exports of
                                        export :: exports ->
                                            ModuleExportLocation export <|
                                                ModuleExportPath moduleTerm.name [] exports moduleTerm.imports moduleTerm.declarations

                                        [] ->
                                            ModuleNameLocation moduleTerm.name <|
                                                ModuleNamePath moduleTerm.exports moduleTerm.imports moduleTerm.declarations

                ModuleImportLocation _ _ ->
                    location

                ModuleExportLocation _ _ ->
                    location

                ModuleNameLocation _ _ ->
                    location

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

                        ApplyPattern [] ->
                            location

                        ApplyPattern (first :: rest) ->
                            PatternLocation first <|
                                ApplyPatternPath [] up rest

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

                ModuleImportLocation importTerm path ->
                    case path of
                        ModuleImportPath moduleName exports left right declarations ->
                            let
                                newImport =
                                    "Left"
                            in
                            ModuleImportLocation newImport <|
                                ModuleImportPath moduleName exports left (importTerm :: right) declarations

                ModuleExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName left right imports declarations ->
                            let
                                newExport =
                                    { name = "left"
                                    , exportConstructors = False
                                    }
                            in
                            ModuleExportLocation newExport <|
                                ModuleExportPath moduleName left (export :: right) imports declarations

                ModuleNameLocation _ _ ->
                    location

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl moduleName exports imports left right ->
                            let
                                declaration =
                                    { pattern = NamePattern "ml"
                                    , expr = Leaf "1"
                                    }

                                newMDecl =
                                    ModuleValueDeclaration declaration
                            in
                            ModuleDeclLocation newMDecl <|
                                ModuleDecl moduleName exports imports left (mDecl :: right)

                TypePatternLocation tPattern path ->
                    location

                TypeExprLocation typeExpr path ->
                    location

                PatternLocation pattern path ->
                    case path of
                        ApplyPatternPath left up right ->
                            PatternLocation (NamePattern "L") <|
                                ApplyPatternPath left up (pattern :: right)

                        DeclPattern _ _ ->
                            location

                        ModuleDeclPattern _ _ ->
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

                ModuleImportLocation importTerm path ->
                    case path of
                        ModuleImportPath moduleName exports left right declarations ->
                            let
                                newImport =
                                    "Right"
                            in
                            ModuleImportLocation newImport <|
                                ModuleImportPath moduleName exports (importTerm :: left) right declarations

                ModuleExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName left right imports declarations ->
                            let
                                newExport =
                                    { name = "right"
                                    , exportConstructors = False
                                    }
                            in
                            ModuleExportLocation newExport <|
                                ModuleExportPath moduleName (export :: left) right imports declarations

                ModuleNameLocation _ _ ->
                    location

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl moduleName exports imports left right ->
                            let
                                declaration =
                                    { pattern = NamePattern "mr"
                                    , expr = Leaf "1"
                                    }

                                newMDecl =
                                    ModuleValueDeclaration declaration
                            in
                            ModuleDeclLocation newMDecl <|
                                ModuleDecl moduleName exports imports (mDecl :: left) right

                TypePatternLocation tPattern path ->
                    location

                TypeExprLocation typeExpr path ->
                    location

                PatternLocation pattern path ->
                    case path of
                        ApplyPatternPath left up right ->
                            PatternLocation (NamePattern "R") <|
                                ApplyPatternPath (pattern :: left) up right

                        DeclPattern _ _ ->
                            location

                        ModuleDeclPattern _ _ ->
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

                ModuleImportLocation importTerm path ->
                    case path of
                        ModuleImportPath moduleName exports (l :: left) right declarations ->
                            ModuleImportLocation importTerm <|
                                ModuleImportPath moduleName exports left (l :: right) declarations

                        ModuleImportPath moduleName exports [] right declarations ->
                            location

                ModuleExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName (l :: left) right imports declarations ->
                            ModuleExportLocation export <|
                                ModuleExportPath moduleName left (l :: right) imports declarations

                        ModuleExportPath moduleName [] right imports declarations ->
                            location

                ModuleNameLocation _ _ ->
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
                        ApplyPatternPath [] _ _ ->
                            location

                        ApplyPatternPath (l :: left) up right ->
                            PatternLocation pattern <|
                                ApplyPatternPath left up (l :: right)

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
                        ModuleDecl _ _ _ [] _ ->
                            location

                        ModuleDecl moduleName exports imports (l :: left) right ->
                            ModuleDeclLocation moduleDecl <|
                                ModuleDecl moduleName exports imports left (l :: right)

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

                ModuleImportLocation importTerm path ->
                    case path of
                        ModuleImportPath moduleName exports left (r :: right) declarations ->
                            ModuleImportLocation importTerm <|
                                ModuleImportPath moduleName exports (r :: left) right declarations

                        ModuleImportPath moduleName exports left [] declarations ->
                            location

                ModuleExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName left (r :: right) imports declarations ->
                            ModuleExportLocation export <|
                                ModuleExportPath moduleName (r :: left) right imports declarations

                        ModuleExportPath moduleName left [] imports declarations ->
                            location

                ModuleNameLocation _ _ ->
                    location

                ModuleDeclLocation moduleDecl path ->
                    case path of
                        ModuleDecl _ _ _ _ [] ->
                            location

                        ModuleDecl moduleName exports imports left (r :: right) ->
                            ModuleDeclLocation moduleDecl <|
                                ModuleDecl moduleName exports imports (r :: left) right

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
                        ApplyPatternPath _ _ [] ->
                            location

                        ApplyPatternPath left up (r :: right) ->
                            PatternLocation pattern <|
                                ApplyPatternPath (r :: left) up right

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

                ModuleImportLocation _ _ ->
                    location

                ModuleExportLocation _ _ ->
                    location

                ModuleNameLocation _ _ ->
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
                ModuleLocation moduleTerm ->
                    let
                        newTypeDecl =
                            ModuleTypeDeclaration { pattern = NameTypePattern "A", typeExpr = NameType "String" }
                    in
                    ModuleDeclLocation newTypeDecl <|
                        ModuleDecl moduleTerm.name moduleTerm.exports moduleTerm.imports [] moduleTerm.declarations

                ModuleDeclLocation currentDecl path ->
                    case path of
                        ModuleDecl moduleName exports imports left right ->
                            let
                                newTypeDecl =
                                    ModuleTypeDeclaration { pattern = NameTypePattern "A", typeExpr = NameType "String" }
                            in
                            ModuleDeclLocation newTypeDecl <|
                                ModuleDecl moduleName exports imports left (currentDecl :: right)

                -- All of these remaining ones, could absolutely accept a InsertTypeDecl, you just need to go up
                -- until you get to the module declarations and *then* insert the new type declaration.
                -- In theory a lot of the module header stuff could be done at any location and perhaps with a
                -- dialog. For example, I really think we should have 'add import' and particularly when on a module declaration
                -- we should have either "add to export list", or "remove from export list", though not quite sure what to do about
                -- constructors in that scenario?
                ModuleImportLocation _ _ ->
                    location

                ModuleExportLocation _ _ ->
                    location

                ModuleNameLocation _ _ ->
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
                        ApplyPatternPath [] _ [] ->
                            -- An impossible state that we shouldn't get into see ApplyPath above.
                            editorState

                        ApplyPatternPath (l :: left) up right ->
                            { clipBoard = Just editorState.location
                            , location =
                                PatternLocation l <| ApplyPatternPath left up right
                            }

                        ApplyPatternPath [] up (r :: right) ->
                            { clipBoard = Just editorState.location
                            , location =
                                PatternLocation r <| ApplyPatternPath [] up right
                            }

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
                        ModuleDecl moduleName exports imports [] [] ->
                            let
                                -- If you cut the last module declaration in theory we could
                                -- land on the right most import, and if there are none of them
                                -- then the right most export, and if none of them then the modulename,
                                -- basically the same as 'goLeft'. But for now I don't think it's terrible
                                -- to go up to the module.
                                moduleTerm =
                                    { name = moduleName
                                    , exports = exports
                                    , imports = imports
                                    , declarations = []
                                    }
                            in
                            { clipBoard = Just editorState.location
                            , location = ModuleLocation moduleTerm
                            }

                        ModuleDecl moduleName exports imports (l :: left) right ->
                            { clipBoard = Just editorState.location
                            , location =
                                ModuleDeclLocation l <|
                                    ModuleDecl moduleName exports imports left right
                            }

                        ModuleDecl moduleName exports imports [] (r :: right) ->
                            { clipBoard = Just editorState.location
                            , location =
                                ModuleDeclLocation r <|
                                    ModuleDecl moduleName exports imports [] right
                            }

                ModuleImportLocation importTerm path ->
                    let
                        newLocation =
                            case path of
                                ModuleImportPath moduleName exports (l :: left) right declarations ->
                                    ModuleImportLocation l <|
                                        ModuleImportPath moduleName exports left right declarations

                                ModuleImportPath moduleName exports [] (r :: right) declarations ->
                                    ModuleImportLocation r <|
                                        ModuleImportPath moduleName exports [] right declarations

                                ModuleImportPath moduleName exports [] [] (decl :: declarations) ->
                                    ModuleDeclLocation decl <|
                                        ModuleDecl moduleName exports [] [] declarations

                                -- So note, we do not ever go on to the export list, this seems an unlikely natural
                                -- thing to do if you're deleting the last import and there are no declarations.
                                -- It may be that actually people do wish to do that, because we're the bascially doing
                                -- is deleting everything, if that is the case obviously we could add it here.
                                ModuleImportPath moduleName exports [] [] [] ->
                                    ModuleNameLocation moduleName <|
                                        ModuleNamePath exports [] []
                    in
                    { clipBoard = Just editorState.location
                    , location = newLocation
                    }

                ModuleExportLocation export path ->
                    let
                        newLocation =
                            case path of
                                ModuleExportPath moduleName left (r :: right) imports declarations ->
                                    ModuleExportLocation r <|
                                        ModuleExportPath moduleName left right imports declarations

                                ModuleExportPath moduleName (l :: left) [] imports declarations ->
                                    ModuleExportLocation l <|
                                        ModuleExportPath moduleName left [] imports declarations

                                ModuleExportPath moduleName [] [] imports (decl :: declarations) ->
                                    ModuleDeclLocation decl <|
                                        ModuleDecl moduleName [] imports [] declarations

                                ModuleExportPath moduleName [] [] (imp :: imports) [] ->
                                    ModuleImportLocation imp <|
                                        ModuleImportPath moduleName [] [] imports []

                                ModuleExportPath moduleName [] [] [] [] ->
                                    ModuleNameLocation moduleName <|
                                        ModuleNamePath [] [] []
                    in
                    { clipBoard = Just editorState.location
                    , location = newLocation
                    }

                ModuleNameLocation _ _ ->
                    editorState

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


pasteAction : Action
pasteAction =
    let
        updateState editorState =
            case editorState.clipBoard of
                Nothing ->
                    editorState

                Just clipBoard ->
                    case ( editorState.location, clipBoard ) of
                        ( ExprLocation _ path, ExprLocation expr _ ) ->
                            { editorState
                                | location = ExprLocation expr path
                            }

                        ( DeclLocation _ path, DeclLocation valueDecl _ ) ->
                            { editorState
                                | location = DeclLocation valueDecl path
                            }

                        ( PatternLocation _ path, PatternLocation pattern _ ) ->
                            { editorState
                                | location = PatternLocation pattern path
                            }

                        ( TypeExprLocation _ path, TypeExprLocation typeExpr _ ) ->
                            { editorState
                                | location = TypeExprLocation typeExpr path
                            }

                        ( TypePatternLocation _ path, TypePatternLocation typePattern _ ) ->
                            { editorState
                                | location = TypePatternLocation typePattern path
                            }

                        ( ModuleDeclLocation _ path, ModuleDeclLocation moduleDeclaration _ ) ->
                            { editorState
                                | location = ModuleDeclLocation moduleDeclaration path
                            }

                        ( ModuleLocation _, ModuleLocation moduleExpr ) ->
                            { editorState
                                | location = ModuleLocation moduleExpr
                            }

                        ( ModuleImportLocation _ path, ModuleImportLocation importTerm _ ) ->
                            { editorState
                                | location = ModuleImportLocation importTerm path
                            }

                        ( ModuleExportLocation _ path, ModuleExportLocation export _ ) ->
                            { editorState
                                | location = ModuleExportLocation export path
                            }

                        ( ModuleNameLocation _ path, ModuleNameLocation name _ ) ->
                            { editorState
                                | location = ModuleNameLocation name path
                            }

                        -- I'm just spelling all of these out so that I get a compiler warning to update the above
                        -- (and below) whenever we add a location kind.
                        ( ExprLocation _ _, _ ) ->
                            editorState

                        ( DeclLocation _ _, _ ) ->
                            editorState

                        ( PatternLocation _ _, _ ) ->
                            editorState

                        ( TypeExprLocation _ _, _ ) ->
                            editorState

                        ( TypePatternLocation _ _, _ ) ->
                            editorState

                        ( ModuleImportLocation _ _, _ ) ->
                            editorState

                        ( ModuleExportLocation _ _, _ ) ->
                            editorState

                        ( ModuleNameLocation _ _, _ ) ->
                            editorState

                        ( ModuleDeclLocation _ _, _ ) ->
                            editorState

                        ( ModuleLocation _, _ ) ->
                            editorState
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
                        ExprLocation expr _ ->
                            case expr of
                                Leaf _ ->
                                    focusLeafBox

                                Apply _ ->
                                    Cmd.none

                                Let _ _ ->
                                    Cmd.none

                        PatternLocation pattern _ ->
                            case pattern of
                                NamePattern _ ->
                                    focusLeafBox

                                ApplyPattern _ ->
                                    Cmd.none

                        TypePatternLocation typePattern _ ->
                            case typePattern of
                                NameTypePattern _ ->
                                    focusLeafBox

                        TypeExprLocation typeExpr _ ->
                            case typeExpr of
                                NameType _ ->
                                    focusLeafBox

                        ModuleImportLocation _ _ ->
                            focusLeafBox

                        ModuleExportLocation _ _ ->
                            focusLeafBox

                        ModuleNameLocation _ _ ->
                            focusLeafBox

                        ModuleDeclLocation _ _ ->
                            Cmd.none

                        ModuleLocation _ ->
                            Cmd.none

                        DeclLocation _ _ ->
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

                        ModuleNameLocation _ path ->
                            updateLocation <|
                                ModuleNameLocation content path

                        ModuleImportLocation _ path ->
                            updateLocation <|
                                ModuleImportLocation content path

                        ModuleExportLocation export path ->
                            updateLocation <|
                                ModuleExportLocation { export | name = content } path

                        ExprLocation _ _ ->
                            model

                        PatternLocation _ _ ->
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
                , Background.color themeColor2
                , Font.color themeColor5
                ]
                [ header model.editorState
                , viewLocation model.editorState.location
                ]
    in
    { title = "Tree source editing with Elm"
    , body = [ Element.layout [] body ]
    }



-- These are the colours of the elm logo
-- #60b5cc -- light blue
-- #f0ad00 -- orange
-- #7fd13b -- green
-- #5a6378 -- grey


themeColor1 : Element.Color
themeColor1 =
    -- Element.rgb255 34 91 120
    Element.rgb255 240 173 0


themeColor2 : Element.Color
themeColor2 =
    -- Element.rgb255 104 175 195
    Element.rgb255 90 99 120


themeColor3 : Element.Color
themeColor3 =
    -- Element.rgb255 125 53 26
    Element.rgb255 127 209 59


themeColor4 : Element.Color
themeColor4 =
    -- Element.rgb255 240 142 89
    Element.rgb255 96 181 204


themeColor5 : Element.Color
themeColor5 =
    Element.rgb255 207 222 230


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
                [ Background.color themeColor2
                , Border.color themeColor1
                , Border.width 2
                , Element.padding 5
                , Border.rounded 5
                , Element.alpha opacity
                , Font.color themeColor1
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
        , makeButton pasteAction "Paste"
        ]


indentElement : Element.Attr decorative msg
indentElement =
    Element.moveRight 20


viewKeyword : String -> Element msg
viewKeyword word =
    el
        [ Font.color themeColor1
        , Font.bold
        ]
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


layoutDeclaration : Element msg -> Element msg -> Element msg
layoutDeclaration declPattern declExpr =
    Element.column
        []
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
    List.map viewDeclaration declarations



-- We should use a scheme to distinguish between laying out a tree segment from the
-- already viewed constitient parts, and viewing something from scratch, eg.
-- viewDeclaration : Declaration -> Element msg
-- which would use
-- layoutDeclaration : Element msg -> Element msg -> Element msg


viewDeclaration : ValueDeclaration -> Element msg
viewDeclaration decl =
    layoutDeclaration
        (viewPattern decl.pattern)
        (viewExpr decl.expr)


viewPattern : Pattern -> Element msg
viewPattern pattern =
    case pattern of
        NamePattern s ->
            text s

        ApplyPattern patterns ->
            viewApply <| List.map viewPattern patterns


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
                    layoutDeclaration (viewPattern pattern) viewed
            in
            viewDeclPath child up

        ModuleDeclExpr pattern up ->
            let
                child =
                    layoutDeclaration (viewPattern pattern) viewed
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
        ApplyPatternPath left up right ->
            let
                leftViewed =
                    List.map viewPattern left

                rightViewed =
                    List.map viewPattern right

                children =
                    upList leftViewed viewed rightViewed

                child =
                    viewApply children
            in
            viewPatternPath child up

        DeclPattern up expr ->
            let
                child =
                    layoutDeclaration viewed <| viewExpr expr
            in
            viewDeclPath child up

        ModuleDeclPattern up expr ->
            let
                child =
                    layoutDeclaration viewed <| viewExpr expr
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
        , Element.el
            [ indentElement ]
            viewedTypeExpr
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
            viewDeclaration declaration

        ModuleTypeDeclaration declaration ->
            layoutTypeDeclaration
                (viewTypePattern declaration.pattern)
                (viewTypeExpr declaration.typeExpr)


viewModuleDeclPath : Element msg -> ModuleDeclPath -> Element msg
viewModuleDeclPath viewed path =
    case path of
        ModuleDecl moduleName exports imports left right ->
            let
                leftViewed =
                    List.map viewModuleDeclaration left

                rightViewed =
                    List.map viewModuleDeclaration right

                declarations =
                    upList leftViewed viewed rightViewed

                heading =
                    viewModuleHeading moduleName exports imports
            in
            layoutModule heading declarations


viewExport : Export -> Element msg
viewExport export =
    case export.exportConstructors of
        False ->
            text export.name

        True ->
            Element.row
                [ Element.spacing 2 ]
                [ text export.name
                , viewPunctuation "(..)"
                ]


layoutExports : List (Element msg) -> Element msg
layoutExports exports =
    case exports of
        [] ->
            Element.el
                [ Font.light ]
                (text "No exports")

        [ only ] ->
            Element.row
                [ indentElement ]
                [ viewPunctuation "("
                , only
                , viewPunctuation ")"
                ]

        first :: others ->
            let
                viewedFirst =
                    Element.row
                        [ Element.spacing 5 ]
                        [ viewPunctuation "(", first ]

                viewOther other =
                    Element.row
                        [ Element.spacing 5 ]
                        [ viewPunctuation ",", other ]

                viewedOthers =
                    List.map viewOther others

                closing =
                    viewPunctuation ")"
            in
            Element.column
                [ indentElement ]
                (viewedFirst :: viewedOthers ++ [ closing ])


viewExports : List Export -> Element msg
viewExports exports =
    layoutExports <| List.map viewExport exports


layoutImport : Element msg -> Element msg
layoutImport importTerm =
    Element.row
        [ Element.spacing 5 ]
        [ viewKeyword "import"
        , importTerm
        ]


viewImport : Import -> Element msg
viewImport importTerm =
    layoutImport <| text importTerm


layoutImports : List (Element msg) -> Element msg
layoutImports =
    Element.column
        []


viewImports : List Import -> Element msg
viewImports imports =
    layoutImports <| List.map viewImport imports


layoutModuleHeading : Element msg -> Element msg -> Element msg -> Element msg
layoutModuleHeading nameRow exports imports =
    Element.column
        []
        [ nameRow
        , exports
        , imports
        ]


layoutNameRow : Element msg -> Element msg
layoutNameRow viewed =
    Element.row
        [ Element.spacing 5 ]
        [ viewKeyword "module"
        , viewed
        , viewKeyword "exposing"
        ]


viewNameRow : ModuleName -> Element msg
viewNameRow moduleName =
    layoutNameRow <| text moduleName


viewModuleHeading : ModuleName -> List Export -> List Import -> Element msg
viewModuleHeading moduleName exports imports =
    layoutModuleHeading
        (viewNameRow moduleName)
        (viewExports exports)
        (viewImports imports)


layoutModule : Element msg -> List (Element msg) -> Element msg
layoutModule heading mDecls =
    Element.column
        [ Element.spacing 10 ]
        (heading :: mDecls)


viewHighlighted : Element Msg -> Element Msg
viewHighlighted viewed =
    el
        [ Border.width 2
        , Border.rounded 5
        , Border.color themeColor5
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
        ModuleLocation moduleTerm ->
            let
                declarations =
                    List.map viewModuleDeclaration moduleTerm.declarations

                heading =
                    viewModuleHeading moduleTerm.name moduleTerm.exports moduleTerm.imports
            in
            viewHighlighted <| layoutModule heading declarations

        ModuleImportLocation importTerm path ->
            let
                viewed =
                    layoutImport <| viewFocusedLeaf importTerm
            in
            case path of
                ModuleImportPath moduleName exports left right declarations ->
                    let
                        viewedLeft =
                            List.map viewImport left

                        viewedRight =
                            List.map viewImport right

                        heading =
                            layoutModuleHeading
                                (viewNameRow moduleName)
                                (viewExports exports)
                                (layoutImports <| upList viewedLeft viewed viewedRight)
                    in
                    layoutModule heading <| List.map viewModuleDeclaration declarations

        ModuleExportLocation exportTerm path ->
            let
                -- TODO: we must be able to export the constructors or not
                viewed =
                    viewFocusedLeaf exportTerm.name
            in
            case path of
                ModuleExportPath moduleName left right imports declarations ->
                    let
                        viewedLeft =
                            List.map viewExport left

                        viewedRight =
                            List.map viewExport right

                        heading =
                            layoutModuleHeading
                                (viewNameRow moduleName)
                                (layoutExports <| upList viewedLeft viewed viewedRight)
                                (viewImports imports)
                    in
                    layoutModule heading <| List.map viewModuleDeclaration declarations

        ModuleNameLocation moduleName path ->
            case path of
                ModuleNamePath exports imports declarations ->
                    let
                        viewed =
                            viewFocusedLeaf moduleName

                        heading =
                            layoutModuleHeading
                                (layoutNameRow viewed)
                                (viewExports exports)
                                (viewImports imports)
                    in
                    layoutModule heading <| List.map viewModuleDeclaration declarations

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
                        viewDeclaration declaration
            in
            viewDeclPath viewed path

        PatternLocation pattern path ->
            let
                viewed =
                    case pattern of
                        NamePattern s ->
                            viewFocusedLeaf s

                        _ ->
                            viewHighlighted <| viewPattern pattern
            in
            viewPatternPath viewed path


idAttribute : String -> Element.Attribute msg
idAttribute s =
    Element.htmlAttribute <| Attributes.id s


classAttribute : String -> Element.Attribute msg
classAttribute s =
    Element.htmlAttribute <| Attributes.class s
