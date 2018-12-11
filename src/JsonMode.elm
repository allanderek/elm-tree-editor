module JsonMode exposing
    ( Node(..)
    , newBuffer
    , view
    )

import Browser
import BufferMessage exposing (BufferMsg)
import Element exposing (Element, text)
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
    = ListNode
    | ObjectNode
    | FieldNode


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
    { title = "Json Tree Editing"
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

        Branch ListNode children ->
            layoutList <| viewChildren children

        Branch ObjectNode [ ListChild children ] ->
            layoutObject <| List.map viewTerm children

        Branch ObjectNode children ->
            ViewUtils.viewErrored (layoutObject <| viewChildren children)

        Branch FieldNode children ->
            layoutField <| viewChildren children


viewChild : Child Node -> Element msg
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


viewChildren : List (Child Node) -> List (Element msg)
viewChildren =
    List.map viewChild


layoutList : List (Element msg) -> Element msg
layoutList elements =
    let
        layoutListElement punctuation element =
            Element.row
                [ ViewUtils.usualSpacing ]
                [ ViewUtils.viewPunctuation punctuation
                , element
                ]
    in
    case elements of
        [] ->
            ViewUtils.viewPunctuation "[]"

        first :: rest ->
            -- Actually now that I think about it, this is very 'elmy' layout of list,
            -- for json maybe we don't actually want this.
            Element.column
                [ ViewUtils.usualSpacing ]
                [ layoutListElement "[" first
                , Element.column
                    [ ViewUtils.usualSpacing ]
                    (List.map (layoutListElement ",") rest)
                , ViewUtils.viewPunctuation "]"
                ]


layoutObject : List (Element msg) -> Element msg
layoutObject elements =
    case elements of
        [] ->
            ViewUtils.viewPunctuation "{}"

        [ one ] ->
            Element.row
                [ ViewUtils.usualSpacing ]
                [ ViewUtils.viewPunctuation "{"
                , one
                , ViewUtils.viewPunctuation "}"
                ]

        _ ->
            -- We need to not output the comma at the end of the *last* field.
            let
                displayField f =
                    Element.row
                        []
                        [ f, ViewUtils.viewPunctuation "," ]

                fields =
                    List.map displayField elements
            in
            Element.column
                [ ViewUtils.usualSpacing ]
                [ ViewUtils.viewPunctuation "{"
                , Element.row
                    []
                    [ ViewUtils.indentation
                    , Element.column
                        [ ViewUtils.usualSpacing ]
                        fields
                    ]
                , ViewUtils.viewPunctuation "}"
                ]


layoutField : List (Element msg) -> Element msg
layoutField elements =
    case elements of
        [] ->
            Element.none

        first :: rest ->
            Element.row
                [ ViewUtils.usualSpacing ]
                [ first
                , ViewUtils.viewPunctuation ":"

                -- Obviously rest should be exactly one.
                , Element.row [] rest
                ]


viewLocation : Location Node -> Element BufferMsg
viewLocation location =
    let
        hole =
            case location.current of
                Leaf s ->
                    ViewUtils.viewFocusedLeaf s

                Branch _ _ ->
                    ViewUtils.viewHighlighted <| viewTerm location.current
    in
    viewPath hole location.path


viewPath : Element msg -> Path Node -> Element msg
viewPath viewed path =
    case path of
        Top ->
            viewed

        SingleChildPath bpath ->
            viewBranchPath viewed bpath

        OptionalChildPath bpath ->
            viewBranchPath viewed bpath

        ListChildPath left bpath right ->
            case bpath.kind of
                ListNode ->
                    -- TODO: This just assumes that bpath.right and bpath.left are empty.
                    -- we need to check that and error if not.
                    let
                        elements =
                            Types.mapUpList viewTerm left viewed right

                        hole =
                            layoutList elements
                    in
                    viewPath hole bpath.up

                ObjectNode ->
                    -- TODO: This just assumes that bpath.right and bpath.left are empty.
                    -- we need to check that and error if not.
                    let
                        elements =
                            Types.mapUpList viewTerm left viewed right

                        hole =
                            layoutObject elements
                    in
                    viewPath hole bpath.up

                FieldNode ->
                    -- This shouldn't happen a field node should not have a list child, it
                    -- has just two singleton children.
                    -- TODO: This is absolutely not right though we need to view the left and right.
                    viewBranchPath (ViewUtils.viewErrored viewed) bpath


viewBranchPath : Element msg -> Types.BranchPath Node -> Element msg
viewBranchPath viewed bpath =
    let
        children =
            Types.mapUpList viewChild bpath.left viewed bpath.right

        hole =
            case bpath.kind of
                ListNode ->
                    layoutList children

                ObjectNode ->
                    layoutObject children

                FieldNode ->
                    layoutField children
    in
    viewPath hole bpath.up
