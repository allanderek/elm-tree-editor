module ElmMode exposing
    ( Node(..)
    , newBuffer
    , view
    )

import BufferMessage exposing (BufferMsg)
import Element exposing (Element, text)
import Element.Font as Font
import GenericActions
import Types
    exposing
        ( Buffer
        , Child(..)
        , Location
        , Path(..)
        , Term(..)
        )
import ViewUtils


type Node
    = Let
    | Apply
    | Decl


newBuffer : Location Node -> Buffer Node
newBuffer location =
    { state =
        { location = location
        , clipBoard = Nothing
        }
    , actions = GenericActions.defaultActions
    , keys = GenericActions.defaultKeys
    }


view : Buffer Node -> { title : String, body : Element BufferMsg }
view buffer =
    let
        clipBoard =
            viewClipboard buffer

        mainContent =
            viewLocation buffer.state.location
    in
    { title = "Elm Tree Editing"
    , body = ViewUtils.body buffer clipBoard mainContent
    }


viewClipboard : Buffer Node -> Element msg
viewClipboard buffer =
    case buffer.state.clipBoard of
        Nothing ->
            Element.none

        Just term ->
            viewTerm term


viewTerm : Term Node -> Element msg
viewTerm term =
    case term of
        Leaf s ->
            text s

        Branch Let [ ListChild declarations, Singleton expr ] ->
            layoutLet
                (List.map viewTerm declarations)
                (viewTerm expr)

        Branch Let children ->
            layoutLetError <| viewChildren children

        Branch Apply [ ListChild children ] ->
            layoutApply <| List.map viewTerm children

        Branch Apply children ->
            layoutApplyError <| viewChildren children

        Branch Decl [ Singleton pattern, Singleton expr ] ->
            layoutDecl (viewTerm pattern) (viewTerm expr)

        Branch Decl children ->
            layoutDeclError <| viewChildren children


viewChild : Child (Term Node) -> Element msg
viewChild child =
    case child of
        Singleton term ->
            viewTerm term

        OptionalChild term ->
            viewTerm term

        ListChild terms ->
            Element.column
                []
                (List.map viewTerm terms)


viewChildren : List (Child (Term Node)) -> List (Element msg)
viewChildren =
    List.map viewChild


viewLocation : Location Node -> Element BufferMsg
viewLocation =
    ViewUtils.viewLocation viewTerm viewPath


viewPath : Element msg -> Path Node -> Element msg
viewPath =
    ViewUtils.createViewPath viewBranchPath viewTerm


layoutLet : List (Element msg) -> Element msg -> Element msg
layoutLet declarations expr =
    Element.column
        [ ViewUtils.usualSpacing ]
        [ ViewUtils.viewKeyword "let"
        , ViewUtils.indentElement <|
            Element.column
                [ ViewUtils.usualSpacing ]
                declarations
        , ViewUtils.viewKeyword "in"
        , Element.el
            []
            expr
        ]


layoutLetError : List (Element msg) -> Element msg
layoutLetError children =
    let
        -- TODO: We can probably do better here, for example if there are exactly
        -- two children, or if there are more we can take the last one as the expr.
        declarations =
            children

        expr =
            Element.none
    in
    ViewUtils.viewErrored <| layoutLet declarations expr


layoutApply : List (Element msg) -> Element msg
layoutApply =
    Element.row
        [ Element.spacing 8
        , Element.width Element.fill
        ]


layoutApplyError : List (Element msg) -> Element msg
layoutApplyError children =
    ViewUtils.viewErrored <| layoutApply children


layoutDecl : Element msg -> Element msg -> Element msg
layoutDecl pattern expr =
    Element.column
        []
        [ Element.row
            [ ViewUtils.usualSpacing ]
            [ Element.el
                [ Element.width Element.fill
                , Font.color ViewUtils.themeColor4
                ]
                pattern
            , ViewUtils.viewPunctuation "="
            ]
        , ViewUtils.indentElement expr
        ]


layoutDeclError : List (Element msg) -> Element msg
layoutDeclError children =
    case children of
        [ pattern, expr ] ->
            ViewUtils.viewErrored <|
                layoutDecl pattern expr

        [ pattern ] ->
            ViewUtils.viewErrored <|
                layoutDecl pattern Element.none

        [] ->
            ViewUtils.viewErrored <|
                layoutDecl Element.none Element.none

        pattern :: rest ->
            ViewUtils.viewErrored <|
                layoutDecl pattern <|
                    Element.row
                        [ ViewUtils.usualSpacing ]
                        rest


viewBranchPath : Types.Child (Element msg) -> Types.BranchPath Node -> Element msg
viewBranchPath viewedChild bpath =
    let
        children =
            Types.mapUpList (Types.mapChild viewTerm) bpath.left viewedChild bpath.right

        hole =
            case bpath.kind of
                Let ->
                    case children of
                        [ ListChild declarations, Singleton expr ] ->
                            layoutLet declarations expr

                        _ ->
                            layoutLetError <|
                                Types.extractFromChildren children

                Apply ->
                    case children of
                        [ ListChild funAndArgs ] ->
                            layoutApply funAndArgs

                        _ ->
                            layoutApplyError <| Types.extractFromChildren children

                Decl ->
                    case children of
                        [ Singleton pattern, Singleton expr ] ->
                            layoutDecl pattern expr

                        _ ->
                            layoutDeclError <| Types.extractFromChildren children
    in
    viewPath hole bpath.up
