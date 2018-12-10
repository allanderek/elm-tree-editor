module Main exposing (commaSeparated, layoutApply, layoutCase, layoutCaseBranch, layoutConstructorDecl, layoutDeclaration, layoutExport, layoutExports, layoutImport, layoutImports, layoutLambda, layoutLet, layoutModule, layoutModuleHeading, layoutNameRow, layoutTerm, layoutTypeAlias, layoutTypeDecl, viewBranchPath, viewChild, viewClipboard, viewErrored, viewFocusedLeaf, viewHighlighted, viewLocation, viewPath, viewTerm)


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


layoutApply : List (Element msg) -> Element msg
layoutApply elements =
    Element.row
        [ Element.spacing 4
        , Element.width Element.fill
        ]
        elements


layoutTypeAlias : Element msg -> Element msg -> Element msg
layoutTypeAlias viewedTypePattern viewedTypeExpr =
    Element.column
        []
        [ Element.row
            [ Element.spacing 5 ]
            [ viewKeyword "type"
            , viewKeyword "alias"
            , viewedTypePattern
            , viewPunctuation "="
            ]
        , Element.row
            []
            [ indentation
            , viewedTypeExpr
            ]
        ]


layoutTypeDecl : Element msg -> List (Element msg) -> Element msg
layoutTypeDecl typePattern constructors =
    let
        viewedConstructors =
            case constructors of
                [] ->
                    []

                first :: rest ->
                    let
                        viewedFirst =
                            Element.row
                                [ Element.spacing 5 ]
                                [ viewPunctuation "="
                                , first
                                ]

                        viewRest c =
                            Element.row
                                [ Element.spacing 5 ]
                                [ viewPunctuation "|"
                                , c
                                ]
                    in
                    viewedFirst :: List.map viewRest rest
    in
    Element.column
        []
        [ Element.row
            [ Element.spacing 5 ]
            [ viewKeyword "type"
            , typePattern
            ]
        , Element.row
            []
            [ indentation
            , Element.column
                [ Element.spacing 5 ]
                viewedConstructors
            ]
        ]


layoutConstructorDecl : Element msg -> List (Element msg) -> Element msg
layoutConstructorDecl name arguments =
    Element.row
        [ Element.spacing 5 ]
        (name :: arguments)


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
                        [ indentation
                        , Element.row
                            []
                            [ viewKeyword "exposing"
                            , viewedExports
                            ]
                        ]

                True ->
                    Element.row
                        []
                        [ indentation
                        , Element.column
                            [ indentElement ]
                            [ viewKeyword "exposing"
                            , viewedExports
                            ]
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


layoutImports : List (Element msg) -> Element msg
layoutImports =
    Element.column
        []


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


viewClipboard : EditorState -> Element msg
viewClipboard editorState =
    case editorState.clipBoard of
        Nothing ->
            text "Clipboard empty"

        Just term ->
            viewTerm term


viewLocation : Location -> Element Msg
viewLocation location =
    let
        viewed =
            case location.current of
                Leaf s ->
                    viewFocusedLeaf s

                Branch _ _ ->
                    viewHighlighted <| viewTerm location.current
    in
    viewPath viewed location.path


viewBranchPath : List (Element msg) -> BranchPath -> Element msg
viewBranchPath viewed branchPath =
    let
        newViewed =
            layoutTerm branchPath.kind <| mapUpList viewChild branchPath.left viewed branchPath.right
    in
    viewPath newViewed branchPath.up


viewPath : Element msg -> Path -> Element msg
viewPath viewed path =
    case path of
        Top ->
            viewed

        SingleChildPath branchPath ->
            viewBranchPath [ viewed ] branchPath

        OptionalChildPath branchPath ->
            viewBranchPath [ viewed ] branchPath

        ListChildPath left branchPath right ->
            let
                subViewed =
                    mapUpList viewTerm left viewed right
            in
            viewBranchPath subViewed branchPath


viewChild : Child Term -> List (Element msg)
viewChild child =
    case child of
        Singleton term ->
            [ viewTerm term ]

        OptionalChild term ->
            [ viewTerm term ]

        ListChild children ->
            List.map viewTerm children


viewTerm : Term -> Element msg
viewTerm term =
    case term of
        Leaf s ->
            text s

        Branch kind childTerms ->
            layoutTerm kind <| List.map viewChild childTerms


viewErrored : String -> Element msg
viewErrored s =
    text ("Error: " ++ s)


layoutTerm : NodeKind -> List (List (Element msg)) -> Element msg
layoutTerm kind childBranches =
    case kind of
        Let ->
            case childBranches of
                [ declarations, [ inExpr ] ] ->
                    layoutLet declarations inExpr

                _ ->
                    viewErrored "invalid let"

        Apply ->
            case childBranches of
                [ expressions ] ->
                    layoutApply expressions

                _ ->
                    viewErrored "invalid apply"

        Decl ->
            case childBranches of
                [ [ pattern ], [ expr ] ] ->
                    layoutDeclaration pattern expr

                _ ->
                    viewErrored "invalid declaration"
