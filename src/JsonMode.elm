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

        Branch ListNode [ ListChild children ] ->
            layoutList <| List.map viewTerm children

        Branch ListNode children ->
            layoutListError <| viewChildren children

        Branch ObjectNode [ ListChild children ] ->
            layoutObject <| List.map viewTerm children

        Branch ObjectNode children ->
            layoutObjectError <| viewChildren children

        Branch FieldNode [ Singleton field, Singleton value ] ->
            layoutField (viewTerm field) (viewTerm value)

        Branch FieldNode children ->
            layoutFieldError <| viewChildren children


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


layoutListError : List (Element msg) -> Element msg
layoutListError children =
    ViewUtils.viewErrored <| layoutList children


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
            -- TODO: We need to not output the comma at the end of the *last* field.
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


layoutObjectError : List (Element msg) -> Element msg
layoutObjectError children =
    ViewUtils.viewErrored <| layoutObject children


layoutField : Element msg -> Element msg -> Element msg
layoutField field value =
    Element.row
        [ ViewUtils.usualSpacing ]
        [ field
        , ViewUtils.viewPunctuation ":"
        , value
        ]


layoutFieldError : List (Element msg) -> Element msg
layoutFieldError elements =
    let
        fieldElement =
            case elements of
                [] ->
                    layoutField Element.none Element.none

                [ one ] ->
                    layoutField one Element.none

                [ field, value ] ->
                    layoutField field value

                field :: rest ->
                    let
                        value =
                            Element.row [] rest
                    in
                    layoutField field value
    in
    ViewUtils.viewErrored fieldElement


viewLocation : Location Node -> Element BufferMsg
viewLocation =
    ViewUtils.viewLocation viewTerm viewPath


viewPath : Element msg -> Path Node -> Element msg
viewPath =
    ViewUtils.createViewPath viewBranchPath viewTerm


viewBranchPath : Types.Child (Element msg) -> Types.BranchPath Node -> Element msg
viewBranchPath viewedChild bpath =
    let
        children =
            Types.mapUpList (Types.mapChild viewTerm) bpath.left viewedChild bpath.right

        hole =
            case bpath.kind of
                ListNode ->
                    case children of
                        [ ListChild elements ] ->
                            layoutList elements

                        _ ->
                            layoutListError <|
                                Types.extractFromChildren children

                ObjectNode ->
                    case children of
                        [ ListChild elements ] ->
                            layoutObject elements

                        _ ->
                            layoutObjectError <|
                                Types.extractFromChildren children

                FieldNode ->
                    case children of
                        [ Singleton field, Singleton value ] ->
                            layoutField field value

                        _ ->
                            layoutFieldError <| Types.extractFromChildren children
    in
    viewPath hole bpath.up
