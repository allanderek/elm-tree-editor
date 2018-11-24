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


type ApplyKind
    = Applicative
    | ListApply
    | TupleApply


type Expr
    = Leaf String
    | Apply ApplyKind (List Expr)
    | Let (List ValueDeclaration) Expr
    | Case Expr (List CaseBranch)
    | Lambda CaseBranch



-- It may seem that ValueDeclaration and CaseBranch could be one and the same
-- however ultimately ValueDeclaration will have a type associated with it.


type alias CaseBranch =
    { pattern : Pattern
    , expr : Expr
    }


type alias ValueDeclaration =
    { pattern : Pattern
    , expr : Expr
    }


type Pattern
    = NamePattern String
    | ApplyPattern ApplyKind (List Pattern)


type ExprPath
    = ApplyPath ApplyKind (List Expr) ExprPath (List Expr)
    | LetExpr (List ValueDeclaration) ExprPath
    | CaseExprPath ExprPath (List CaseBranch)
    | BranchExprPath Pattern CaseBranchPath
    | LambdaExprPath Pattern ExprPath
    | DeclExpr Pattern DeclPath
    | ModuleDeclExpr Pattern ModuleDeclPath


type DeclPath
    = LetDecl (List ValueDeclaration) ExprPath (List ValueDeclaration) Expr


type CaseBranchPath
    = CaseBranchPath Expr (List CaseBranch) ExprPath (List CaseBranch)


type ModuleDeclPath
    = ModuleDecl ModuleName (List Export) (List Import) (List ModuleDeclaration) (List ModuleDeclaration)


type ModuleImportPath
    = ModuleImportPath ModuleName (List Export) (List Import) (List Import) (List ModuleDeclaration)


type ExportPath
    = ModuleExportPath ModuleName (List Export) (List Export) (List Import) (List ModuleDeclaration)
    | ImportExposingPath ModuleName (Maybe ModuleName) (List Export) ModuleImportPath (List Export)


type ModuleNamePath
    = ModuleNamePath (List Export) (List Import) (List ModuleDeclaration)
    | ModuleImportNamePath ModuleImportPath (Maybe ModuleName) (List Export)
    | ModuleImportAsPath ImportName ModuleImportPath (List Export)


type PatternPath
    = ApplyPatternPath ApplyKind (List Pattern) PatternPath (List Pattern)
    | CasePattern CaseBranchPath Expr
    | LambdaPatternPath ExprPath Expr
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


type alias Import =
    { name : ImportName
    , asName : Maybe String
    , exports : List Export
    }


type alias ModuleName =
    String


type alias ImportName =
    ModuleName


type alias Module =
    { name : ModuleName
    , exports : List Export
    , imports : List Import
    , declarations : List ModuleDeclaration
    }


type Location
    = ExprLocation Expr ExprPath
    | CaseLocation CaseBranch CaseBranchPath
    | DeclLocation ValueDeclaration DeclPath
    | PatternLocation Pattern PatternPath
    | TypeExprLocation TypeExpr TypeExprPath
    | TypePatternLocation TypePattern TypePatternPath
    | ModuleImportLocation Import ModuleImportPath
    | ExportLocation Export ExportPath
    | ModuleNameLocation ModuleName ModuleNamePath
    | ModuleDeclLocation ModuleDeclaration ModuleDeclPath
    | ModuleLocation Module


initialLocation : Location
initialLocation =
    let
        expr =
            Let
                [ { pattern = ApplyPattern Applicative [ NamePattern "Just", NamePattern "a" ], expr = Apply Applicative [ Leaf "Just", Leaf "1" ] }
                , { pattern = NamePattern "b", expr = Leaf "2" }
                , { pattern = NamePattern "c", expr = Leaf "3" }
                , { pattern = NamePattern "d", expr = Lambda { pattern = NamePattern "x", expr = Leaf "4" } }
                , { pattern = ApplyPattern ListApply [ NamePattern "a", NamePattern "b", NamePattern "c" ]
                  , expr = Apply ListApply [ Leaf "1", Leaf "2", Leaf "3" ]
                  }
                ]
                (Case
                    (Apply Applicative [ Leaf "Other.blah", Leaf "strong" ])
                    [ { pattern = ApplyPattern Applicative [ NamePattern "Just", NamePattern "a" ], expr = Apply Applicative [ Leaf "a", Leaf "b", Leaf "c", Leaf "d" ] }
                    , { pattern = NamePattern "Nothing", expr = Apply ListApply [] }
                    ]
                )

        moduleDecl =
            ModuleValueDeclaration { pattern = NamePattern "main", expr = expr }

        typeDecl =
            ModuleTypeDeclaration { pattern = NameTypePattern "Person", typeExpr = NameType "String" }

        moduleTerm =
            { name = "Main"
            , exports =
                [ { name = "main", exportConstructors = False }
                , { name = "init", exportConstructors = False }
                , { name = "update", exportConstructors = False }
                , { name = "Msg", exportConstructors = True }
                ]
            , imports =
                [ { name = "Html", asName = Nothing, exports = [] }
                , { name = "Html.Attributes", asName = Just "Attributes", exports = [ { name = "class", exportConstructors = False } ] }
                , { name = "Html.Events", asName = Just "Attributes", exports = [ { name = "onClick", exportConstructors = False } ] }
                , { name = "Maybe", asName = Nothing, exports = [ { name = "Maybe", exportConstructors = True } ] }
                ]
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

                ModuleNameLocation moduleName path ->
                    case path of
                        ModuleNamePath _ _ _ ->
                            location

                        ModuleImportNamePath _ _ _ ->
                            location

                        ModuleImportAsPath importName up exports ->
                            ModuleNameLocation importName <|
                                ModuleImportNamePath up (Just moduleName) exports

                ExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName [] right imports declarations ->
                            ModuleNameLocation moduleName <|
                                ModuleNamePath (export :: right) imports declarations

                        ModuleExportPath moduleName (l :: left) right imports declarations ->
                            ExportLocation l <|
                                ModuleExportPath moduleName left (export :: right) imports declarations

                        ImportExposingPath moduleName Nothing [] up right ->
                            ModuleNameLocation moduleName <|
                                ModuleImportNamePath up Nothing (export :: right)

                        ImportExposingPath moduleName (Just asName) [] up right ->
                            ModuleNameLocation asName <|
                                ModuleImportAsPath moduleName up (export :: right)

                        ImportExposingPath moduleName mAsName (l :: left) up right ->
                            ExportLocation l <|
                                ImportExposingPath moduleName mAsName left up (export :: right)

                ModuleImportLocation importTerm path ->
                    case path of
                        ModuleImportPath moduleName exports (l :: left) right declarations ->
                            ModuleImportLocation l <|
                                ModuleImportPath moduleName exports left (importTerm :: right) declarations

                        ModuleImportPath moduleName exports [] right declarations ->
                            case List.reverse exports of
                                [] ->
                                    ModuleNameLocation moduleName <|
                                        ModuleNamePath [] (importTerm :: right) declarations

                                export :: leftExports ->
                                    ExportLocation export <|
                                        ModuleExportPath moduleName leftExports [] (importTerm :: right) declarations

                ModuleDeclLocation mDecl path ->
                    case path of
                        ModuleDecl moduleName exports imports (l :: left) right ->
                            ModuleDeclLocation l <| ModuleDecl moduleName exports imports left (mDecl :: right)

                        ModuleDecl moduleName exports imports [] right ->
                            case List.reverse imports of
                                importTerm :: leftImports ->
                                    ModuleImportLocation importTerm <|
                                        ModuleImportPath moduleName exports leftImports [] (mDecl :: right)

                                [] ->
                                    case List.reverse exports of
                                        export :: leftExports ->
                                            ExportLocation export <|
                                                ModuleExportPath moduleName leftExports [] imports (mDecl :: right)

                                        [] ->
                                            ModuleNameLocation moduleName <| ModuleNamePath [] [] (mDecl :: right)

                ExprLocation expr path ->
                    case path of
                        ApplyPath _ [] _ _ ->
                            location

                        ApplyPath applyKind (l :: left) up right ->
                            ExprLocation l <| ApplyPath applyKind left up (expr :: right)

                        LetExpr [] _ ->
                            location

                        LetExpr (l :: left) up ->
                            DeclLocation l <| LetDecl left up [] expr

                        LambdaExprPath pattern up ->
                            PatternLocation pattern <|
                                LambdaPatternPath up expr

                        CaseExprPath _ _ ->
                            location

                        BranchExprPath pattern up ->
                            PatternLocation pattern <| CasePattern up expr

                        DeclExpr pattern up ->
                            PatternLocation pattern <| DeclPattern up expr

                        ModuleDeclExpr pattern up ->
                            PatternLocation pattern <| ModuleDeclPattern up expr

                CaseLocation branch path ->
                    case path of
                        CaseBranchPath expr [] up right ->
                            ExprLocation expr <|
                                CaseExprPath up right

                        CaseBranchPath expr (l :: left) up right ->
                            CaseLocation l <| CaseBranchPath expr left up (branch :: right)

                DeclLocation declaration path ->
                    case path of
                        LetDecl [] _ _ _ ->
                            location

                        LetDecl (l :: left) up right expr ->
                            DeclLocation l <| LetDecl left up (declaration :: right) expr

                PatternLocation pattern path ->
                    case path of
                        ApplyPatternPath _ [] _ _ ->
                            location

                        ApplyPatternPath applyKind (l :: left) up right ->
                            PatternLocation l <| ApplyPatternPath applyKind left up (pattern :: right)

                        LambdaPatternPath _ _ ->
                            location

                        -- In theory we could go left onto the previous case/declaration for these two.
                        CasePattern _ _ ->
                            location

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
                            ExportLocation export <|
                                ModuleExportPath moduleName [] exports imports declarations

                        ModuleImportNamePath up Nothing [] ->
                            location

                        ModuleImportNamePath up Nothing (export :: exports) ->
                            ExportLocation export <|
                                ImportExposingPath moduleName Nothing [] up exports

                        ModuleImportNamePath up (Just asName) exports ->
                            ModuleNameLocation asName <|
                                ModuleImportAsPath moduleName up exports

                        ModuleImportAsPath importName up [] ->
                            location

                        ModuleImportAsPath importName up (export :: exports) ->
                            ExportLocation export <|
                                ImportExposingPath importName (Just moduleName) [] up exports

                ExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName left [] [] [] ->
                            location

                        ModuleExportPath moduleName left [] [] (decl :: declarations) ->
                            let
                                exports =
                                    List.reverse (export :: left)
                            in
                            ModuleDeclLocation decl <|
                                ModuleDecl moduleName exports [] [] declarations

                        ModuleExportPath moduleName left [] (imp :: imports) declarations ->
                            let
                                exports =
                                    List.reverse (export :: left)
                            in
                            ModuleImportLocation imp <|
                                ModuleImportPath moduleName exports [] imports declarations

                        ModuleExportPath moduleName left (r :: right) imports declarations ->
                            ExportLocation r <|
                                ModuleExportPath moduleName (export :: left) right imports declarations

                        ImportExposingPath importName mAsName left up [] ->
                            location

                        ImportExposingPath importName mAsName left up (r :: right) ->
                            ExportLocation r <|
                                ImportExposingPath importName mAsName (r :: left) up right

                ModuleImportLocation importTerm path ->
                    case path of
                        ModuleImportPath moduleName exports left [] [] ->
                            location

                        ModuleImportPath moduleName exports left (r :: right) declarations ->
                            ModuleImportLocation r <|
                                ModuleImportPath moduleName exports (importTerm :: left) right declarations

                        ModuleImportPath moduleName exports left [] (decl :: declarations) ->
                            let
                                imports =
                                    List.reverse (importTerm :: left)
                            in
                            ModuleDeclLocation decl <|
                                ModuleDecl moduleName exports imports [] declarations

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
                        ApplyPatternPath _ _ _ [] ->
                            location

                        ApplyPatternPath applyKind left up (r :: right) ->
                            PatternLocation r <| ApplyPatternPath applyKind (pattern :: left) up right

                        LambdaPatternPath up expr ->
                            ExprLocation expr <|
                                LambdaExprPath pattern up

                        CasePattern up expr ->
                            ExprLocation expr <| BranchExprPath pattern up

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

                CaseLocation branch path ->
                    case path of
                        CaseBranchPath _ _ _ [] ->
                            location

                        CaseBranchPath expr left up (r :: right) ->
                            CaseLocation r <| CaseBranchPath expr (branch :: left) up right

                ExprLocation expr path ->
                    case path of
                        ApplyPath _ _ _ [] ->
                            location

                        ApplyPath applyKind left up (r :: right) ->
                            ExprLocation r <| ApplyPath applyKind (expr :: left) up right

                        LetExpr _ _ ->
                            location

                        LambdaExprPath _ _ ->
                            location

                        CaseExprPath up [] ->
                            location

                        CaseExprPath up (b :: branches) ->
                            CaseLocation b <|
                                CaseBranchPath expr [] up branches

                        -- Both of these could in theory go right on to the next
                        -- case/declaration.
                        BranchExprPath _ _ ->
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

                ExportLocation export path ->
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

                        ImportExposingPath importName mAsName left up right ->
                            let
                                importTerm =
                                    { name = importName
                                    , asName = mAsName
                                    , exports = upList left export right
                                    }
                            in
                            ModuleImportLocation importTerm up

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

                        ModuleImportNamePath up mAsName exports ->
                            let
                                importTerm =
                                    { name = moduleName
                                    , asName = mAsName
                                    , exports = exports
                                    }
                            in
                            ModuleImportLocation importTerm up

                        ModuleImportAsPath importName up exports ->
                            let
                                importTerm =
                                    { name = importName
                                    , asName = Just moduleName
                                    , exports = exports
                                    }
                            in
                            ModuleImportLocation importTerm up

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
                        ApplyPatternPath applyKind left up right ->
                            let
                                newPattern =
                                    ApplyPattern applyKind <| upList left pattern right
                            in
                            PatternLocation newPattern up

                        LambdaPatternPath up expr ->
                            ExprLocation (Lambda { pattern = pattern, expr = expr }) up

                        CasePattern up expr ->
                            let
                                caseBranch =
                                    { pattern = pattern
                                    , expr = expr
                                    }
                            in
                            CaseLocation caseBranch up

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

                CaseLocation branch path ->
                    case path of
                        CaseBranchPath expr left up right ->
                            let
                                branches =
                                    upList left branch right

                                caseExpr =
                                    Case expr branches
                            in
                            ExprLocation caseExpr up

                ExprLocation expr path ->
                    case path of
                        ApplyPath applyKind left up right ->
                            let
                                args =
                                    upList left expr right
                            in
                            ExprLocation (Apply applyKind args) up

                        LetExpr declarations up ->
                            let
                                letExpr =
                                    Let (List.reverse declarations) expr
                            in
                            ExprLocation letExpr up

                        LambdaExprPath pattern up ->
                            ExprLocation (Lambda { pattern = pattern, expr = expr }) up

                        CaseExprPath up branches ->
                            let
                                caseExpr =
                                    Case expr branches
                            in
                            ExprLocation caseExpr up

                        BranchExprPath pattern up ->
                            let
                                branch =
                                    { pattern = pattern
                                    , expr = expr
                                    }
                            in
                            CaseLocation branch up

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
                                            ExportLocation export <|
                                                ModuleExportPath moduleTerm.name [] exports moduleTerm.imports moduleTerm.declarations

                                        [] ->
                                            ModuleNameLocation moduleTerm.name <|
                                                ModuleNamePath moduleTerm.exports moduleTerm.imports moduleTerm.declarations

                ModuleImportLocation importTerm path ->
                    ModuleNameLocation importTerm.name <|
                        ModuleImportNamePath path importTerm.asName importTerm.exports

                ExportLocation _ _ ->
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

                        ApplyPattern _ [] ->
                            location

                        ApplyPattern applyKind (first :: rest) ->
                            PatternLocation first <|
                                ApplyPatternPath applyKind [] up rest

                DeclLocation declaration up ->
                    -- Note this is a break from convention we're going into the expression
                    -- which is kind of not the first child of the declaration, but my argument is
                    -- that often you will wish to modify the expression in a declaration without
                    -- modifying the name.
                    ExprLocation declaration.expr <| DeclExpr declaration.pattern up

                CaseLocation branch up ->
                    -- Similar to the above we figure going 'down' into a case, you more likely wish to
                    -- modify the expression.
                    ExprLocation branch.expr <| BranchExprPath branch.pattern up

                ExprLocation expr up ->
                    case expr of
                        Leaf _ ->
                            location

                        Apply _ [] ->
                            location

                        Apply applyKind (first :: rest) ->
                            ExprLocation first <| ApplyPath applyKind [] up rest

                        Let [] inExpr ->
                            -- This should not happen but I guess it's kind of possible if someone is deleting declarations
                            -- the question is whether, when someone deletes the last declaration we just coalasce the expression or not.
                            -- We could actually just do this regardless of how many declarations there are. I suspect at some point I might
                            -- have GoDownLeft and GoDownRight.
                            ExprLocation inExpr <| LetExpr [] up

                        Let (first :: rest) inExpr ->
                            DeclLocation first <| LetDecl [] up rest inExpr

                        Case matchExpr branches ->
                            ExprLocation matchExpr <|
                                CaseExprPath up branches

                        Lambda branch ->
                            -- Again the convention is that you go to the left most child,
                            -- but I think it's far more likely you wish to edit the expression.
                            ExprLocation branch.expr <|
                                LambdaExprPath branch.pattern up

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
                                    { name = "Left"
                                    , asName = Nothing
                                    , exports = []
                                    }
                            in
                            ModuleImportLocation newImport <|
                                ModuleImportPath moduleName exports left (importTerm :: right) declarations

                ExportLocation export path ->
                    let
                        newExport =
                            { name = "left"
                            , exportConstructors = False
                            }
                    in
                    case path of
                        ModuleExportPath moduleName left right imports declarations ->
                            ExportLocation newExport <|
                                ModuleExportPath moduleName left (export :: right) imports declarations

                        ImportExposingPath importName mAsName left up right ->
                            ExportLocation newExport <|
                                ImportExposingPath importName mAsName left up (export :: right)

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
                        ApplyPatternPath applyKind left up right ->
                            PatternLocation (NamePattern "L") <|
                                ApplyPatternPath applyKind left up (pattern :: right)

                        DeclPattern _ _ ->
                            location

                        CasePattern _ _ ->
                            location

                        ModuleDeclPattern _ _ ->
                            location

                        LambdaPatternPath _ _ ->
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

                CaseLocation branch path ->
                    case path of
                        CaseBranchPath matchExpr left up right ->
                            let
                                newBranch =
                                    { pattern = NamePattern "_"
                                    , expr = Leaf "Nothing"
                                    }
                            in
                            CaseLocation newBranch <|
                                CaseBranchPath matchExpr left up (branch :: right)

                ExprLocation expr path ->
                    case path of
                        ApplyPath applyKind left up right ->
                            ExprLocation (Leaf "1") <|
                                ApplyPath applyKind left up (expr :: right)

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

                        LambdaExprPath _ _ ->
                            location

                        CaseExprPath _ _ ->
                            location

                        BranchExprPath _ _ ->
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
                                    { name = "Right"
                                    , asName = Nothing
                                    , exports = []
                                    }
                            in
                            ModuleImportLocation newImport <|
                                ModuleImportPath moduleName exports (importTerm :: left) right declarations

                ExportLocation export path ->
                    let
                        newExport =
                            { name = "right"
                            , exportConstructors = False
                            }
                    in
                    case path of
                        ModuleExportPath moduleName left right imports declarations ->
                            ExportLocation newExport <|
                                ModuleExportPath moduleName (export :: left) right imports declarations

                        ImportExposingPath importName mAsName left up right ->
                            ExportLocation newExport <|
                                ImportExposingPath importName mAsName (export :: left) up right

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
                        ApplyPatternPath applyKind left up right ->
                            PatternLocation (NamePattern "R") <|
                                ApplyPatternPath applyKind (pattern :: left) up right

                        DeclPattern _ _ ->
                            location

                        LambdaPatternPath _ _ ->
                            location

                        CasePattern _ _ ->
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

                CaseLocation branch path ->
                    case path of
                        CaseBranchPath matchExpr left up right ->
                            let
                                newBranch =
                                    { pattern = NamePattern "R"
                                    , expr = Leaf "Nothing"
                                    }
                            in
                            CaseLocation newBranch <|
                                CaseBranchPath matchExpr (branch :: left) up right

                ExprLocation expr path ->
                    case path of
                        ApplyPath applyKind left up right ->
                            let
                                newExpr =
                                    Leaf "e"
                            in
                            ExprLocation newExpr <|
                                ApplyPath applyKind (expr :: left) up right

                        LetExpr _ _ ->
                            -- In theory we could interpret this as 'insert a new declaration at the bottom of the list of declarations'
                            -- I suspect this is something of a common task, it's easy enough from here anyway you simply go Right->insertRight.
                            -- In general I suspect adding a new declaration to the inner most enclosing 'let' even if you're not actually
                            -- on the outer-most 'inExpr' will be common, and we should probably have a separate task for that.
                            location

                        LambdaExprPath _ _ ->
                            location

                        BranchExprPath _ _ ->
                            -- Similarly we could interpret this as 'add a new case'.
                            location

                        CaseExprPath _ _ ->
                            -- And also here, 'add a new case'
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

                ExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName (l :: left) right imports declarations ->
                            ExportLocation export <|
                                ModuleExportPath moduleName left (l :: right) imports declarations

                        ModuleExportPath moduleName [] right imports declarations ->
                            location

                        ImportExposingPath importName mAsName (l :: left) up right ->
                            ExportLocation export <|
                                ImportExposingPath importName mAsName left up (l :: right)

                        ImportExposingPath importName mAsName [] up right ->
                            location

                ModuleNameLocation _ _ ->
                    location

                ExprLocation expr path ->
                    case path of
                        ApplyPath applyKind [] _ _ ->
                            location

                        ApplyPath applyKind (l :: left) up right ->
                            ExprLocation expr <|
                                ApplyPath applyKind left up (l :: right)

                        LetExpr [] _ ->
                            location

                        LetExpr (l :: left) up ->
                            location

                        LambdaExprPath _ _ ->
                            location

                        CaseExprPath _ _ ->
                            location

                        BranchExprPath _ _ ->
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

                CaseLocation branch path ->
                    case path of
                        CaseBranchPath _ [] _ _ ->
                            location

                        CaseBranchPath matchExpr (l :: left) up right ->
                            CaseLocation branch <|
                                CaseBranchPath matchExpr left up (l :: right)

                PatternLocation pattern path ->
                    case path of
                        ApplyPatternPath _ [] _ _ ->
                            location

                        ApplyPatternPath applyKind (l :: left) up right ->
                            PatternLocation pattern <|
                                ApplyPatternPath applyKind left up (l :: right)

                        LambdaPatternPath _ _ ->
                            location

                        CasePattern _ _ ->
                            location

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

                ExportLocation export path ->
                    case path of
                        ModuleExportPath moduleName left (r :: right) imports declarations ->
                            ExportLocation export <|
                                ModuleExportPath moduleName (r :: left) right imports declarations

                        ModuleExportPath moduleName left [] imports declarations ->
                            location

                        ImportExposingPath importName mAsName left up (r :: right) ->
                            ExportLocation export <|
                                ImportExposingPath importName mAsName (r :: left) up right

                        ImportExposingPath importName mAsName left up [] ->
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
                        ApplyPatternPath _ _ _ [] ->
                            location

                        ApplyPatternPath applyKind left up (r :: right) ->
                            PatternLocation pattern <|
                                ApplyPatternPath applyKind (r :: left) up right

                        DeclPattern up expr ->
                            location

                        LambdaPatternPath _ _ ->
                            location

                        CasePattern _ _ ->
                            location

                        ModuleDeclPattern up expr ->
                            location

                DeclLocation declaration path ->
                    case path of
                        LetDecl left up [] expr ->
                            location

                        LetDecl left up (r :: right) expr ->
                            DeclLocation declaration <| LetDecl (r :: left) up right expr

                CaseLocation branch path ->
                    case path of
                        CaseBranchPath _ _ _ [] ->
                            location

                        CaseBranchPath matchExpr left up (r :: right) ->
                            CaseLocation branch <|
                                CaseBranchPath matchExpr (r :: left) up right

                ExprLocation expr path ->
                    case path of
                        ApplyPath _ _ _ [] ->
                            location

                        ApplyPath applyKind left up (r :: right) ->
                            ExprLocation expr <| ApplyPath applyKind (r :: left) up right

                        LetExpr _ _ ->
                            location

                        DeclExpr _ _ ->
                            location

                        LambdaExprPath _ _ ->
                            location

                        CaseExprPath _ _ ->
                            location

                        BranchExprPath _ _ ->
                            location

                        ModuleDeclExpr _ _ ->
                            location

        updateState =
            updateStateLocation updateLocation
    in
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


type PromoteExpression
    = PromoteLet
    | PromoteCase
    | PromoteList
    | PromoteTuple
    | PromoteApply
    | PromoteLambda


promoteExpression : PromoteExpression -> Action
promoteExpression promote =
    let
        updateLocation location =
            case location of
                ModuleLocation _ ->
                    location

                ModuleImportLocation _ _ ->
                    location

                ExportLocation _ _ ->
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

                CaseLocation _ _ ->
                    location

                ExprLocation expr path ->
                    case ( promote, expr, path ) of
                        -- TODO: In all of these you're putting the new location on the newly created promoted
                        -- expression, but in theory you could keep it on the *current* expression that is likely
                        -- where you want to be anyway, because when you promote an expression you're almost certain to
                        -- want to edit it, so the way it currently is now you'll almost certainly follow the 'promote'
                        -- action with a 'go-down'.
                        ( PromoteLet, Let _ _, _ ) ->
                            location

                        ( PromoteLet, _, LetExpr _ _ ) ->
                            location

                        ( PromoteLet, _, _ ) ->
                            let
                                newDeclaration =
                                    { pattern = NamePattern "n"
                                    , expr = Leaf "3"
                                    }

                                newExpr =
                                    Let [ newDeclaration ] expr
                            in
                            ExprLocation newExpr path

                        ( PromoteCase, _, _ ) ->
                            let
                                newBranch =
                                    { pattern = NamePattern "_"
                                    , expr = expr
                                    }

                                newExpr =
                                    Case (Leaf "True") [ newBranch ]
                            in
                            ExprLocation newExpr path

                        ( PromoteList, _, _ ) ->
                            ExprLocation (Apply ListApply [ expr ]) path

                        ( PromoteTuple, _, _ ) ->
                            ExprLocation (Apply TupleApply [ expr ]) path

                        ( PromoteApply, _, _ ) ->
                            ExprLocation (Apply Applicative [ expr, Leaf "a" ]) path

                        ( PromoteLambda, _, _ ) ->
                            ExprLocation (Lambda { pattern = NamePattern "_", expr = expr }) path

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

                ExportLocation _ _ ->
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

                CaseLocation _ _ ->
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


toggleExportConstructors : Action
toggleExportConstructors =
    let
        updateLocation location =
            case location of
                ExportLocation export path ->
                    let
                        -- Note: we could prevent someone from toggling this if the first character of the name
                        -- is not an upper case character. However, note that if we do that, it's still possible to
                        -- toggle it to export the constructors and *then* change the name to a lower-case character,
                        -- so either we would want to prevent them changing the name or at least allow them to toggle
                        -- exporting constructors *off* regardless fo what the current name is. All-in-all for now I
                        -- don't think this is worth it, so I'm happy to just let the user make that mistake or not.
                        -- I'd be happy to highlight it as an error.
                        newExport =
                            { export | exportConstructors = not export.exportConstructors }
                    in
                    ExportLocation newExport path

                ExprLocation _ _ ->
                    location

                CaseLocation _ _ ->
                    location

                DeclLocation _ _ ->
                    location

                PatternLocation _ _ ->
                    location

                TypeExprLocation _ _ ->
                    location

                TypePatternLocation _ _ ->
                    location

                ModuleImportLocation _ _ ->
                    location

                ModuleNameLocation _ _ ->
                    location

                ModuleDeclLocation _ _ ->
                    location

                ModuleLocation _ ->
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
                        -- Only called from Applicative, since both ListApply and TupleApply can have
                        -- single expressions or even an empty list of expressions.
                        buildPath left up right =
                            case List.isEmpty left && List.isEmpty right of
                                True ->
                                    up

                                False ->
                                    ApplyPath Applicative left up right
                    in
                    case path of
                        ApplyPath Applicative [] up [] ->
                            -- This is an impossible state that we shouldn't get into, we could do some tricks
                            -- for example we could update the location to `ExprLocation expr up` and then recursively
                            -- call `updateState. That would introduce the possiblity of an infinite loop. We could
                            -- alternatively fix up the current location, put that on the clipboard and carry on, so this
                            -- cut action would act like a copy one, but also fix up the impossible state. That's nice, but
                            -- let's just fail in an obvious way so that hopefully the underlying bug gets fixed.
                            editorState

                        ApplyPath Applicative (l :: left) up right ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation l <| buildPath left up right
                            }

                        ApplyPath Applicative [] up (r :: right) ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation r <| buildPath [] up right
                            }

                        -- The difference with list and tuple applies is that they can be just single expressions or even
                        -- empty list of expressions.
                        ApplyPath ListApply [] up [] ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation (Apply ListApply []) up
                            }

                        ApplyPath ListApply (l :: left) up right ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation l <| ApplyPath ListApply left up right
                            }

                        ApplyPath ListApply [] up (r :: right) ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation r <| ApplyPath ListApply [] up right
                            }

                        ApplyPath TupleApply [] up [] ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation (Apply TupleApply []) up
                            }

                        ApplyPath TupleApply (l :: left) up right ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation l <| ApplyPath TupleApply left up right
                            }

                        ApplyPath TupleApply [] up (r :: right) ->
                            { clipBoard = Just editorState.location
                            , location = ExprLocation r <| ApplyPath TupleApply [] up right
                            }

                        LetExpr _ _ ->
                            -- You cannot cut the expression from a let.
                            editorState

                        LambdaExprPath _ _ ->
                            editorState

                        CaseExprPath _ _ ->
                            -- You cannot cut the match expression out of a case
                            editorState

                        BranchExprPath _ _ ->
                            -- You cannot cut the expression out of a branch
                            editorState

                        DeclExpr _ _ ->
                            -- You cannot cut the expression from a declaration.
                            editorState

                        ModuleDeclExpr _ _ ->
                            -- You cannot cut the expression from a module declaration
                            editorState

                CaseLocation branch path ->
                    case path of
                        CaseBranchPath _ [] _ [] ->
                            -- You cannot cut the very last branch of this expression.
                            -- want a way to turn the last branch into *just* its expression, but this is not it.
                            editorState

                        CaseBranchPath matchExpr [] up (r :: right) ->
                            { clipBoard = Just editorState.location
                            , location =
                                CaseLocation r <| CaseBranchPath matchExpr [] up right
                            }

                        CaseBranchPath matchExpr (l :: left) up right ->
                            { clipBoard = Just editorState.location
                            , location =
                                CaseLocation l <| CaseBranchPath matchExpr left up right
                            }

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
                        ApplyPatternPath Applicative [] _ [] ->
                            -- An impossible state that we shouldn't get into see ApplyPath above.
                            editorState

                        ApplyPatternPath Applicative [ left ] up [] ->
                            { clipBoard = Just editorState.location
                            , location = PatternLocation left up
                            }

                        ApplyPatternPath Applicative [] up [ right ] ->
                            { clipBoard = Just editorState.location
                            , location = PatternLocation right up
                            }

                        ApplyPatternPath ListApply [] up [] ->
                            { clipBoard = Just editorState.location
                            , location =
                                PatternLocation (ApplyPattern ListApply []) up
                            }

                        ApplyPatternPath TupleApply [] up [] ->
                            { clipBoard = Just editorState.location
                            , location =
                                PatternLocation (ApplyPattern TupleApply []) up
                            }

                        ApplyPatternPath applyKind (l :: left) up right ->
                            { clipBoard = Just editorState.location
                            , location =
                                PatternLocation l <| ApplyPatternPath applyKind left up right
                            }

                        ApplyPatternPath applyKind [] up (r :: right) ->
                            { clipBoard = Just editorState.location
                            , location =
                                PatternLocation r <| ApplyPatternPath applyKind [] up right
                            }

                        LambdaPatternPath _ _ ->
                            editorState

                        CasePattern _ _ ->
                            editorState

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

                ExportLocation export path ->
                    let
                        newLocation =
                            case path of
                                ModuleExportPath moduleName left (r :: right) imports declarations ->
                                    ExportLocation r <|
                                        ModuleExportPath moduleName left right imports declarations

                                ModuleExportPath moduleName (l :: left) [] imports declarations ->
                                    ExportLocation l <|
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

                                ImportExposingPath importName mAsName (l :: left) up right ->
                                    ExportLocation l <|
                                        ImportExposingPath importName mAsName left up right

                                ImportExposingPath importName mAsName [] up (r :: right) ->
                                    ExportLocation r <|
                                        ImportExposingPath importName mAsName [] up right

                                ImportExposingPath importName (Just asName) [] up [] ->
                                    ModuleNameLocation asName <|
                                        ModuleImportAsPath importName up []

                                ImportExposingPath importName Nothing [] up [] ->
                                    ModuleNameLocation importName <|
                                        ModuleImportNamePath up Nothing []
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

                        ( CaseLocation _ path, CaseLocation branch _ ) ->
                            { editorState
                                | location = CaseLocation branch path
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

                        ( ExportLocation _ path, ExportLocation export _ ) ->
                            { editorState
                                | location = ExportLocation export path
                            }

                        ( ModuleNameLocation _ path, ModuleNameLocation name _ ) ->
                            { editorState
                                | location = ModuleNameLocation name path
                            }

                        -- I'm just spelling all of these out so that I get a compiler warning to update the above
                        -- (and below) whenever we add a location kind.
                        ( ExprLocation _ _, _ ) ->
                            editorState

                        ( CaseLocation _ _, _ ) ->
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

                        ( ExportLocation _ _, _ ) ->
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

                                Apply _ _ ->
                                    Cmd.none

                                Let _ _ ->
                                    Cmd.none

                                Case _ _ ->
                                    Cmd.none

                                Lambda _ ->
                                    Cmd.none

                        PatternLocation pattern _ ->
                            case pattern of
                                NamePattern _ ->
                                    focusLeafBox

                                ApplyPattern _ _ ->
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

                        ExportLocation _ _ ->
                            focusLeafBox

                        ModuleNameLocation _ _ ->
                            focusLeafBox

                        ModuleDeclLocation _ _ ->
                            Cmd.none

                        ModuleLocation _ ->
                            Cmd.none

                        DeclLocation _ _ ->
                            Cmd.none

                        CaseLocation _ _ ->
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

                        ExportLocation export path ->
                            updateLocation <|
                                ExportLocation { export | name = content } path

                        ModuleImportLocation _ _ ->
                            model

                        ExprLocation _ _ ->
                            model

                        PatternLocation _ _ ->
                            model

                        CaseLocation _ _ ->
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


mapUpList : (a -> b) -> List a -> b -> List a -> List b
mapUpList fun left hole right =
    upList (List.map fun left) hole (List.map fun right)


view : Model -> Browser.Document Msg
view model =
    let
        controls =
            Element.column
                []
                [ header model.editorState
                , viewClipboard model.editorState
                ]

        mainContent =
            viewLocation model.editorState.location

        body =
            Element.row
                [ Element.width Element.fill
                , Element.spacing 10
                , Element.padding 10
                , Background.color themeColor2
                , Font.color themeColor5
                ]
                [ Element.el [ Element.width <| Element.fillPortion 3 ] controls
                , Element.el [ Element.width <| Element.fillPortion 7 ] mainContent
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
        [ Element.height Element.fill
        , Element.width Element.fill
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
        , makeButton (promoteExpression PromoteLet) "Let"
        , makeButton (promoteExpression PromoteCase) "Case"
        , makeButton (promoteExpression PromoteList) "List"
        , makeButton (promoteExpression PromoteTuple) "Tuple"
        , makeButton (promoteExpression PromoteApply) "Apply"
        , makeButton (promoteExpression PromoteLambda) "Lambda"
        , makeButton cutAction "Cut"
        , makeButton copyAction "Copy"
        , makeButton pasteAction "Paste"
        , makeButton toggleExportConstructors "(..)"
        ]


indentElement : Element.Attribute msg
indentElement =
    Element.paddingEach { left = 20, right = 0, top = 0, bottom = 0 }


viewKeyword : String -> Element msg
viewKeyword word =
    el
        [ Font.color themeColor1
        , Font.bold
        , Element.padding 3
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

        Apply applyKind children ->
            layoutApply applyKind <| List.map viewExpr children

        Let declarations inExpr ->
            layoutLet (viewDeclarationList declarations) (viewExpr inExpr)

        Case matchExpr branches ->
            layoutCase (viewExpr matchExpr) <| List.map viewCaseBranch branches

        Lambda branch ->
            layoutLambda (viewPattern branch.pattern) (viewExpr branch.expr)


indentation : Element msg
indentation =
    Element.el
        [ Element.width <| Element.px 20
        , Element.height Element.fill

        -- TODO: I could make the indentation just a very subtle off-color, or with a very subtle border, perhaps only on the left.
        , Background.color themeColor2
        ]
        (text "")


layoutLambda : Element msg -> Element msg -> Element msg
layoutLambda pattern expr =
    Element.row
        [ Element.spacing 5 ]
        [ Element.row
            []
            [ viewPunctuation "\\"
            , pattern
            ]
        , viewPunctuation "->"
        , expr
        ]


layoutCaseBranch : Element msg -> Element msg -> Element msg
layoutCaseBranch pattern expr =
    Element.column
        [ Element.spacing 5 ]
        [ Element.row
            [ Element.spacing 5 ]
            [ pattern
            , viewPunctuation "->"
            ]
        , Element.row
            []
            [ indentation, expr ]
        ]


viewCaseBranch : CaseBranch -> Element msg
viewCaseBranch branch =
    layoutCaseBranch (viewPattern branch.pattern) (viewExpr branch.expr)


layoutCase : Element msg -> List (Element msg) -> Element msg
layoutCase matchExpr branches =
    Element.column
        [ Element.spacing 5 ]
        [ Element.row
            []
            [ viewKeyword "case"
            , matchExpr
            , viewKeyword "of"
            ]
        , Element.row
            []
            [ indentation
            , Element.column
                [ Element.spacing 5 ]
                branches
            ]
        ]


layoutLet : List (Element msg) -> Element msg -> Element msg
layoutLet declarations expr =
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

        ApplyPattern applyKind patterns ->
            layoutApply applyKind <| List.map viewPattern patterns


layoutApply : ApplyKind -> List (Element msg) -> Element msg
layoutApply applyKind elements =
    case applyKind of
        Applicative ->
            Element.row
                [ Element.spacing 4
                , Element.width Element.fill
                ]
                elements

        ListApply ->
            commaSeparated
                (viewPunctuation "[")
                elements
                (viewPunctuation "]")

        TupleApply ->
            commaSeparated
                (viewPunctuation "(")
                elements
                (viewPunctuation ")")



-- An alternative and easier way to do this, would be to just go-up until you reach the root
-- and then draw that. It would mean creating the entire source tree, but what you're doing
-- currently is a way of just cutting out that intermediate data structure, but the one you're
-- building will be the same order. So you won't save much unless the tree is large.
-- Another option, is to memoise the view of a tree, we can probably do this somehow with Element.lazy.


viewExprPath : Element msg -> ExprPath -> Element msg
viewExprPath viewed path =
    case path of
        ApplyPath applyKind left up right ->
            let
                children =
                    mapUpList viewExpr left viewed right

                child =
                    layoutApply applyKind children
            in
            viewExprPath child up

        LetExpr declarations up ->
            let
                child =
                    layoutLet (List.reverse <| viewDeclarationList declarations) viewed
            in
            viewExprPath child up

        DeclExpr pattern up ->
            let
                child =
                    layoutDeclaration (viewPattern pattern) viewed
            in
            viewDeclPath child up

        LambdaExprPath pattern up ->
            let
                child =
                    layoutLambda (viewPattern pattern) viewed
            in
            viewExprPath child up

        CaseExprPath up branches ->
            let
                child =
                    layoutCase viewed <| List.map viewCaseBranch branches
            in
            viewExprPath child up

        BranchExprPath pattern up ->
            let
                child =
                    layoutCaseBranch (viewPattern pattern) viewed
            in
            viewCaseBranchPath child up

        ModuleDeclExpr pattern up ->
            let
                child =
                    layoutDeclaration (viewPattern pattern) viewed
            in
            viewModuleDeclPath child up


viewCaseBranchPath : Element msg -> CaseBranchPath -> Element msg
viewCaseBranchPath viewed path =
    case path of
        CaseBranchPath matchExpr left up right ->
            let
                branches =
                    mapUpList viewCaseBranch left viewed right

                child =
                    layoutCase (viewExpr matchExpr) branches
            in
            viewExprPath child up


viewDeclPath : Element msg -> DeclPath -> Element msg
viewDeclPath viewed path =
    case path of
        LetDecl left up right expr ->
            let
                declarations =
                    mapUpList viewDeclaration left viewed right

                child =
                    layoutLet declarations <| viewExpr expr
            in
            viewExprPath child up


viewPatternPath : Element msg -> PatternPath -> Element msg
viewPatternPath viewed path =
    case path of
        ApplyPatternPath applyKind left up right ->
            let
                children =
                    mapUpList viewPattern left viewed right

                child =
                    layoutApply applyKind children
            in
            viewPatternPath child up

        LambdaPatternPath up expr ->
            let
                child =
                    layoutLambda viewed <| viewExpr expr
            in
            viewExprPath child up

        CasePattern up expr ->
            let
                child =
                    layoutCaseBranch viewed <| viewExpr expr
            in
            viewCaseBranchPath child up

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


viewModuleImportPath : Element msg -> ModuleImportPath -> Element msg
viewModuleImportPath viewed path =
    case path of
        ModuleImportPath moduleName exports left right declarations ->
            let
                heading =
                    layoutModuleHeading
                        (viewNameRow moduleName)
                        (viewExports "Nothing exposed" exports)
                        (layoutImports <| mapUpList viewImport left viewed right)
            in
            layoutModule heading <| List.map viewModuleDeclaration declarations


layoutExport : Element msg -> Bool -> Element msg
layoutExport name exportConstructors =
    case exportConstructors of
        False ->
            name

        True ->
            Element.row
                [ Element.spacing 2 ]
                [ name
                , viewPunctuation "(..)"
                ]


viewExport : Export -> Element msg
viewExport export =
    layoutExport (text export.name) export.exportConstructors


layoutExports : String -> List (Element msg) -> Element msg
layoutExports emptyText exports =
    case exports of
        [] ->
            Element.el
                [ indentElement
                , Font.light
                ]
                (text emptyText)

        _ ->
            let
                viewedExports =
                    commaSeparated
                        (viewPunctuation "(")
                        exports
                        (viewPunctuation ")")
            in
            case List.length exports > 3 of
                False ->
                    Element.row
                        []
                        [ viewKeyword "exposing"
                        , viewedExports
                        ]

                True ->
                    Element.column
                        [ indentElement ]
                        [ viewKeyword "exposing"
                        , viewedExports
                        ]


commaSeparated : Element msg -> List (Element msg) -> Element msg -> Element msg
commaSeparated opening elements closing =
    case elements of
        [] ->
            Element.row
                [ Element.spacing 5 ]
                [ opening, closing ]

        [ only ] ->
            Element.row
                [ Element.spacing 5 ]
                [ opening, only, closing ]

        first :: rest ->
            let
                viewedFirst =
                    Element.row
                        [ Element.spacing 5 ]
                        [ opening, first ]

                viewOther other =
                    Element.row
                        [ Element.spacing 5 ]
                        [ viewPunctuation ",", other ]

                viewedOthers =
                    List.map viewOther rest

                viewedAll =
                    viewedFirst :: viewedOthers ++ [ closing ]
            in
            case List.length elements > 3 of
                False ->
                    Element.row
                        []
                        viewedAll

                True ->
                    Element.column
                        [ indentElement ]
                        viewedAll


viewExports : String -> List Export -> Element msg
viewExports emptyText exports =
    layoutExports emptyText <| List.map viewExport exports


layoutImport : Element msg -> Maybe (Element msg) -> Element msg -> Element msg
layoutImport importName mAsName exports =
    let
        asTerm =
            case mAsName of
                Nothing ->
                    Element.none

                Just name ->
                    Element.row
                        [ Element.spacing 5 ]
                        [ viewKeyword "as"
                        , name
                        ]
    in
    Element.column
        [ Element.spacing 5 ]
        [ Element.row
            [ Element.spacing 5 ]
            [ viewKeyword "import"
            , importName
            , asTerm
            ]
        , exports
        ]


viewImport : Import -> Element msg
viewImport importTerm =
    layoutImport
        (text importTerm.name)
        (Maybe.map text importTerm.asName)
        (viewExports "Nothing exposed" importTerm.exports)


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
        (viewExports "No exports" exports)
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
        , Element.paddingEach { right = 10, top = 0, bottom = 0, left = 0 }
        ]
        viewed


viewFocusedLeaf : String -> Element Msg
viewFocusedLeaf contents =
    Input.text
        [ classAttribute "leaf-input"
        , idAttribute leafBoxId
        , Element.width Element.shrink
        , Background.color themeColor2
        , Element.paddingXY 1 1
        , Element.width Element.shrink
        ]
        { onChange = LeafInput
        , text = contents
        , placeholder = Nothing
        , label = Input.labelHidden ""
        }


viewModule : Module -> Element msg
viewModule moduleTerm =
    let
        declarations =
            List.map viewModuleDeclaration moduleTerm.declarations

        heading =
            viewModuleHeading moduleTerm.name moduleTerm.exports moduleTerm.imports
    in
    layoutModule heading declarations


viewClipboard : EditorState -> Element msg
viewClipboard editorState =
    case editorState.clipBoard of
        Nothing ->
            text "Clipboard empty"

        Just (ExprLocation expr _) ->
            viewExpr expr

        Just (CaseLocation branch _) ->
            viewCaseBranch branch

        Just (DeclLocation valueDecl _) ->
            viewDeclaration valueDecl

        Just (PatternLocation pattern _) ->
            viewPattern pattern

        Just (TypeExprLocation typeExpr _) ->
            viewTypeExpr typeExpr

        Just (TypePatternLocation typePattern _) ->
            viewTypePattern typePattern

        Just (ModuleImportLocation importTerm _) ->
            viewImport importTerm

        Just (ExportLocation export _) ->
            viewExport export

        Just (ModuleNameLocation name _) ->
            text name

        Just (ModuleDeclLocation moduleDecl _) ->
            viewModuleDeclaration moduleDecl

        Just (ModuleLocation moduleTerm) ->
            viewModule moduleTerm


viewLocation : Location -> Element Msg
viewLocation location =
    case location of
        ModuleLocation moduleTerm ->
            viewHighlighted <| viewModule moduleTerm

        ModuleImportLocation importTerm path ->
            let
                viewed =
                    viewHighlighted <| viewImport importTerm
            in
            viewModuleImportPath viewed path

        ExportLocation exportTerm path ->
            let
                viewed =
                    layoutExport (viewFocusedLeaf exportTerm.name) exportTerm.exportConstructors
            in
            case path of
                ModuleExportPath moduleName left right imports declarations ->
                    let
                        heading =
                            layoutModuleHeading
                                (viewNameRow moduleName)
                                (layoutExports "Nothing exported" <| mapUpList viewExport left viewed right)
                                (viewImports imports)
                    in
                    layoutModule heading <| List.map viewModuleDeclaration declarations

                ImportExposingPath importName mAsName left up right ->
                    let
                        viewedImport =
                            layoutImport
                                (text importName)
                                (Maybe.map text mAsName)
                                (layoutExports "Nothing exposed" <| mapUpList viewExport left viewed right)
                    in
                    viewModuleImportPath viewedImport up

        ModuleNameLocation moduleName path ->
            let
                viewed =
                    viewFocusedLeaf moduleName
            in
            case path of
                ModuleNamePath exports imports declarations ->
                    let
                        heading =
                            layoutModuleHeading
                                (layoutNameRow viewed)
                                (viewExports "Nothing exported" exports)
                                (viewImports imports)
                    in
                    layoutModule heading <| List.map viewModuleDeclaration declarations

                ModuleImportNamePath up mAsName exports ->
                    let
                        viewedImport =
                            layoutImport
                                viewed
                                (Maybe.map text mAsName)
                                (viewExports "Nothing exposed" exports)
                    in
                    viewModuleImportPath viewedImport up

                ModuleImportAsPath importName up exports ->
                    let
                        viewedImport =
                            layoutImport
                                (text importName)
                                (Just <| viewed)
                                (viewExports "Nothing exposed" exports)
                    in
                    viewModuleImportPath viewedImport up

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

        CaseLocation branch path ->
            let
                viewed =
                    viewHighlighted <|
                        viewCaseBranch branch
            in
            viewCaseBranchPath viewed path

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
