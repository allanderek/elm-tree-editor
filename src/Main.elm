module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
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
        keyDecoder =
            Decode.map KeyPressed <|
                Decode.field "key" Decode.string
    in
    Browser.Events.onKeyPress keyDecoder


type alias ProgramFlags =
    ()


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , elmBuffers : List (Buffer ElmNode)
    , jsonBuffers : List (Buffer JsonNode)
    , currentBuffer : CurrentBuffer
    }


type CurrentBuffer
    = ElmBuffer (Buffer ElmNode)
    | JsonBuffer (Buffer JsonNode)


type ElmNode
    = Let
    | Apply
    | Decl


type JsonNode
    = ListNode
    | ObjectNode
    | FieldNode


type alias Buffer node =
    { state : EditorState node

    -- This means you just have a set list of actions and they each have an 'isAvailable' function.
    -- Alternatively we could have a function from EditorState node -> List (Action node).
    , actions : Dict ActionId (Action node)
    , keys : Dict String ActionId
    }


type alias EditorState node =
    { location : Location node
    , clipBoard : Maybe (Fragment node)
    }


type alias ActionId =
    String


type alias Action node =
    { actionId : ActionId
    , name : String
    , updateState : EditorState node -> EditorState node
    , isAvailable : EditorState node -> Bool
    }



-- The default function for checking whether an action is available or not is just to apply the
-- given update location function and see if the location has changed. Now this obviously may result
-- in a fair amount of unnecessary work for certain actions, in which case we can simply provide a more
-- sophisticated `isAvailable` function.


defaultIsAvailable : (EditorState node -> EditorState node) -> EditorState node -> Bool
defaultIsAvailable updateState state =
    state /= updateState state


updateStateLocation : (Location node -> Location node) -> EditorState node -> EditorState node
updateStateLocation f state =
    { state | location = f state.location }


type alias Fragment node =
    Term node


type alias Location node =
    { current : Term node
    , path : Path node
    }


type Child node
    = Singleton (Term node)
    | ListChild (List (Term node))
    | OptionalChild (Term node)


type Term node
    = Leaf String
    | Branch node (List (Child node))


type Path node
    = Top
    | SingleChildPath (BranchPath node)
    | OptionalChildPath (BranchPath node)
    | ListChildPath (List (Term node)) (BranchPath node) (List (Term node))


type alias BranchPath node =
    { kind : node
    , left : List (Child node)
    , up : Path node
    , right : List (Child node)
    }


defaultKeys : Dict String ActionId
defaultKeys =
    Dict.fromList
        [ ( "ArrowLeft", "goLeft" )
        , ( "ArrowRight", "goRight" )
        , ( "ArrowUp", "goUp" )
        , ( "ArrowDown", "goDown" )
        ]


defaultActions : Dict String (Action node)
defaultActions =
    let
        addAction action =
            Dict.insert action.actionId action

        actions =
            [ genericGoLeft
            , genericGoRight
            , genericGoUp
            , genericGoDown
            ]
    in
    List.foldl addAction Dict.empty actions


init : ProgramFlags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        elmBuffer =
            { state =
                { location =
                    { current = Branch Apply [ ListChild [ Leaf "f", Leaf "a" ] ]
                    , path =
                        SingleChildPath
                            { up = Top
                            , kind = Let
                            , left = [ ListChild [ Branch Decl [ Singleton <| Leaf "a", Singleton <| Leaf "1" ] ] ]
                            , right = []
                            }
                    }
                , clipBoard = Nothing
                }
            , actions = defaultActions
            , keys = defaultKeys
            }

        jsonBuffer =
            { state =
                { location =
                    { current =
                        Branch
                            ObjectNode
                            [ ListChild
                                [ Branch FieldNode [ Singleton <| Leaf "first", Singleton <| Leaf "1" ]
                                , Branch FieldNode [ Singleton <| Leaf "second", Singleton <| Leaf "2" ]
                                , Branch FieldNode [ Singleton <| Leaf "third", Singleton <| Leaf "3" ]
                                ]
                            ]
                    , path = Top
                    }
                , clipBoard = Nothing
                }
            , actions = defaultActions
            , keys = defaultKeys
            }

        {- }
           jsonBuffer =
               { state =
                   { location =
                       { current = Branch FieldNode [ Singleton <| Leaf "f", Singleton <| Leaf "\"a\"" ]
                       , path =
                           ListChildPath
                               []
                               { up = Top
                               , kind = ObjectNode
                               , left =
                                   [ Branch FieldNode [ Singleton <| Leaf "left", Singleton <| Leaf "1" ] ]
                               , right =
                                   [ Branch FieldNode [ Singleton <| Leaf "right", Singleton <| Leaf "2" ] ]
                               }
                               []
                       }
                   , clipBoard = Nothing
                   }
               , actions = defaultActions
               , keys = defaultKeys
               }
        -}
        initialModel =
            { key = key
            , url = url
            , elmBuffers = [ elmBuffer ]
            , jsonBuffers = [ jsonBuffer ]
            , currentBuffer = JsonBuffer jsonBuffer
            }
    in
    withCommands initialModel []


leafBoxId : String
leafBoxId =
    "leaf-box"


focusLeafBox : Cmd Msg
focusLeafBox =
    Task.attempt (always NoOp) (Browser.Dom.focus leafBoxId)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOp
    | KeyPressed String
    | EditorAction ActionId
    | LeafInput String


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

        KeyPressed keyCode ->
            case model.currentBuffer of
                ElmBuffer buffer ->
                    let
                        ( newBuffer, command ) =
                            applyKey keyCode buffer
                    in
                    withCommands { model | currentBuffer = ElmBuffer newBuffer } [ command ]

                JsonBuffer buffer ->
                    let
                        ( newBuffer, command ) =
                            applyKey keyCode buffer
                    in
                    withCommands { model | currentBuffer = JsonBuffer newBuffer } [ command ]

        EditorAction actionId ->
            case model.currentBuffer of
                ElmBuffer buffer ->
                    let
                        ( newBuffer, command ) =
                            applyAction actionId buffer
                    in
                    withCommands { model | currentBuffer = ElmBuffer newBuffer } [ command ]

                JsonBuffer buffer ->
                    let
                        ( newBuffer, command ) =
                            applyAction actionId buffer
                    in
                    withCommands { model | currentBuffer = JsonBuffer newBuffer } [ command ]

        LeafInput content ->
            let
                updateBuffer : Buffer node -> Buffer node
                updateBuffer buffer =
                    case buffer.state.location.current of
                        Leaf _ ->
                            let
                                state =
                                    buffer.state

                                location =
                                    buffer.state.location

                                newLocation =
                                    { location | current = Leaf content }

                                newState =
                                    { state | location = newLocation }
                            in
                            { buffer | state = newState }

                        Branch _ _ ->
                            buffer
            in
            case model.currentBuffer of
                ElmBuffer buffer ->
                    let
                        newBuffer =
                            updateBuffer buffer
                    in
                    withCommands { model | currentBuffer = ElmBuffer newBuffer } []

                JsonBuffer buffer ->
                    let
                        newBuffer =
                            updateBuffer buffer
                    in
                    withCommands { model | currentBuffer = JsonBuffer newBuffer } []


applyKey : String -> Buffer node -> ( Buffer node, Cmd Msg )
applyKey keyCode buffer =
    case Dict.get keyCode buffer.keys of
        Nothing ->
            withCommands buffer []

        Just actionId ->
            applyAction actionId buffer


applyAction : ActionId -> Buffer node -> ( Buffer node, Cmd Msg )
applyAction actionId buffer =
    case Dict.get actionId buffer.actions of
        Nothing ->
            withCommands buffer []

        Just action ->
            let
                newState =
                    action.updateState buffer.state

                newBuffer =
                    { buffer
                        | state = newState
                    }

                command =
                    case newState.location.current of
                        Leaf _ ->
                            focusLeafBox

                        Branch _ _ ->
                            Cmd.none
            in
            withCommands newBuffer [ command ]


withCommands : model -> List (Cmd msg) -> ( model, Cmd msg )
withCommands model commands =
    ( model, Cmd.batch commands )


genericGoLeft : Action node
genericGoLeft =
    let
        updateLocation location =
            let
                branchLeft branchPath current =
                    case branchPath.left of
                        [] ->
                            location

                        (Singleton l) :: left ->
                            { current = l
                            , path =
                                SingleChildPath
                                    { branchPath
                                        | left = left
                                        , right = current :: branchPath.right
                                    }
                            }

                        (OptionalChild l) :: left ->
                            { current = l
                            , path =
                                OptionalChildPath
                                    { branchPath
                                        | left = left
                                        , right = current :: branchPath.right
                                    }
                            }

                        (ListChild listChildren) :: left ->
                            case List.Extra.unconsLast listChildren of
                                Nothing ->
                                    location

                                Just ( last, others ) ->
                                    { current = last
                                    , path =
                                        ListChildPath
                                            (List.reverse others)
                                            { branchPath
                                                | left = left
                                                , right = current :: branchPath.right
                                            }
                                            []
                                    }
            in
            case location.path of
                Top ->
                    location

                SingleChildPath branchPath ->
                    branchLeft branchPath <| Singleton location.current

                OptionalChildPath branchPath ->
                    branchLeft branchPath <| OptionalChild location.current

                ListChildPath [] branchPath right ->
                    branchLeft branchPath <| ListChild (location.current :: right)

                ListChildPath (l :: left) branchPath right ->
                    { current = l
                    , path =
                        ListChildPath left branchPath (location.current :: right)
                    }

        updateState =
            updateStateLocation updateLocation
    in
    { actionId = "goLeft"
    , name = "Left"
    , updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


genericGoRight : Action node
genericGoRight =
    let
        updateLocation location =
            let
                branchRight branchPath current =
                    case branchPath.right of
                        [] ->
                            location

                        (Singleton r) :: right ->
                            { current = r
                            , path =
                                SingleChildPath
                                    { branchPath
                                        | left = current :: branchPath.left
                                        , right = right
                                    }
                            }

                        (OptionalChild r) :: right ->
                            { current = r
                            , path =
                                OptionalChildPath
                                    { branchPath
                                        | left = current :: branchPath.left
                                        , right = right
                                    }
                            }

                        (ListChild listChildren) :: right ->
                            case listChildren of
                                [] ->
                                    location

                                first :: others ->
                                    { current = first
                                    , path =
                                        ListChildPath
                                            []
                                            { branchPath
                                                | left = current :: branchPath.left
                                                , right = right
                                            }
                                            others
                                    }
            in
            case location.path of
                Top ->
                    location

                SingleChildPath branchPath ->
                    branchRight branchPath <| Singleton location.current

                OptionalChildPath branchPath ->
                    branchRight branchPath <| OptionalChild location.current

                ListChildPath left branchPath [] ->
                    branchRight branchPath <| ListChild (List.reverse (location.current :: left))

                ListChildPath left branchPath (r :: right) ->
                    { current = r
                    , path =
                        ListChildPath (location.current :: left) branchPath right
                    }

        updateState =
            updateStateLocation updateLocation
    in
    { actionId = "goRight"
    , name = "Right"
    , updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


genericGoUp : Action node
genericGoUp =
    let
        updateLocation location =
            let
                branchPathUp branchPath current =
                    { path = branchPath.up
                    , current =
                        Branch branchPath.kind <|
                            upList branchPath.left current branchPath.right
                    }
            in
            case location.path of
                Top ->
                    location

                SingleChildPath branchPath ->
                    branchPathUp branchPath <| Singleton location.current

                OptionalChildPath branchPath ->
                    branchPathUp branchPath <| OptionalChild location.current

                ListChildPath left branchPath right ->
                    let
                        current =
                            ListChild <| upList left location.current right
                    in
                    branchPathUp branchPath current

        updateState =
            updateStateLocation updateLocation
    in
    { actionId = "goUp"
    , name = "Up"
    , updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


genericGoDown : Action node
genericGoDown =
    let
        updateLocation location =
            case location.current of
                Leaf _ ->
                    location

                Branch kind [] ->
                    location

                Branch kind (firstChild :: others) ->
                    let
                        upBranchPath =
                            { kind = kind
                            , left = []
                            , up = location.path
                            , right = others
                            }
                    in
                    case firstChild of
                        Singleton child ->
                            { current = child
                            , path = SingleChildPath upBranchPath
                            }

                        OptionalChild child ->
                            { current = child
                            , path = OptionalChildPath upBranchPath
                            }

                        ListChild [] ->
                            location

                        ListChild (first :: rest) ->
                            { current = first
                            , path = ListChildPath [] upBranchPath rest
                            }

        updateState =
            updateStateLocation updateLocation
    in
    { actionId = "goDown"
    , name = "Down"
    , updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }



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
    case model.currentBuffer of
        ElmBuffer _ ->
            let
                body =
                    text "I haven't figured out how to draw elm code yet"
            in
            { title = "Json Tree Editing"
            , body = [ Element.layout [] body ]
            }

        JsonBuffer buffer ->
            viewJsonBuffer buffer


viewJsonBuffer : Buffer JsonNode -> Browser.Document Msg
viewJsonBuffer buffer =
    let
        controls =
            Element.column
                []
                [ header buffer
                , viewClipboard buffer
                ]

        mainContent =
            viewLocation buffer.state.location

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
    { title = "Json Tree Editing"
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


errorColor : Element.Color
errorColor =
    Element.rgb255 222 10 20


header : Buffer node -> Element Msg
header buffer =
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

        makeButton action =
            case action.isAvailable buffer.state of
                True ->
                    button action.name <| Just (EditorAction action.actionId)

                False ->
                    button action.name Nothing
    in
    Element.wrappedRow
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.spaceEvenly
        ]
        (List.map makeButton <| Dict.values buffer.actions)


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


indentation : Element msg
indentation =
    Element.el
        [ Element.width <| Element.px 20
        , Element.height Element.fill

        -- TODO: I could make the indentation just a very subtle off-color, or with a very subtle border, perhaps only on the left.
        , Background.color themeColor2
        ]
        (text "")


idAttribute : String -> Element.Attribute msg
idAttribute s =
    Element.htmlAttribute <| Attributes.id s


classAttribute : String -> Element.Attribute msg
classAttribute s =
    Element.htmlAttribute <| Attributes.class s


viewClipboard : Buffer JsonNode -> Element msg
viewClipboard buffer =
    case buffer.state.clipBoard of
        Nothing ->
            Element.none

        Just term ->
            viewTerm term


viewTerm : Term JsonNode -> Element msg
viewTerm term =
    case term of
        Leaf s ->
            text s

        Branch ListNode children ->
            layoutList <| viewChildren children

        Branch ObjectNode [ ListChild children ] ->
            layoutObject <| List.map viewTerm children

        Branch ObjectNode children ->
            viewErrored (layoutObject <| viewChildren children)

        Branch FieldNode children ->
            layoutField <| viewChildren children


viewChild : Child JsonNode -> Element msg
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


viewChildren : List (Child JsonNode) -> List (Element msg)
viewChildren =
    List.map viewChild


layoutList : List (Element msg) -> Element msg
layoutList elements =
    let
        layoutListElement punctuation element =
            Element.row
                [ usualSpacing ]
                [ viewPunctuation punctuation
                , element
                ]
    in
    case elements of
        [] ->
            viewPunctuation "[]"

        first :: rest ->
            -- Actually now that I think about it, this is very 'elmy' layout of list,
            -- for json maybe we don't actually want this.
            Element.column
                [ usualSpacing ]
                [ layoutListElement "[" first
                , Element.column
                    [ usualSpacing ]
                    (List.map (layoutListElement ",") rest)
                , viewPunctuation "]"
                ]


layoutObject : List (Element msg) -> Element msg
layoutObject elements =
    case elements of
        [] ->
            viewPunctuation "{}"

        [ one ] ->
            Element.row
                [ usualSpacing ]
                [ viewPunctuation "{"
                , one
                , viewPunctuation "}"
                ]

        _ ->
            -- We need to not output the comma at the end of the *last* field.
            let
                displayField f =
                    Element.row
                        []
                        [ f, viewPunctuation "," ]

                fields =
                    List.map displayField elements
            in
            Element.column
                [ usualSpacing ]
                [ viewPunctuation "{"
                , Element.row
                    []
                    [ indentation
                    , Element.column
                        [ usualSpacing ]
                        fields
                    ]
                , viewPunctuation "}"
                ]


layoutField : List (Element msg) -> Element msg
layoutField elements =
    case elements of
        [] ->
            Element.none

        first :: rest ->
            Element.row
                [ usualSpacing ]
                [ first
                , viewPunctuation ":"

                -- Obviously rest should be exactly one.
                , Element.row [] rest
                ]


viewHighlighted : Element msg -> Element msg
viewHighlighted viewed =
    el
        [ Border.width 2
        , Border.rounded 5
        , Border.color themeColor5
        , Element.paddingEach { right = 10, top = 0, bottom = 0, left = 0 }
        ]
        viewed


viewErrored : Element msg -> Element msg
viewErrored viewed =
    el
        [ Border.width 2
        , Border.rounded 5
        , Border.color errorColor
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


viewLocation : Location JsonNode -> Element Msg
viewLocation location =
    let
        hole =
            case location.current of
                Leaf s ->
                    viewFocusedLeaf s

                Branch _ _ ->
                    viewHighlighted <| viewTerm location.current
    in
    viewPath hole location.path


viewPath : Element msg -> Path JsonNode -> Element msg
viewPath viewed path =
    case path of
        Top ->
            viewed

        SingleChildPath bpath ->
            viewBranchPath viewed bpath

        OptionalChildPath bpath ->
            viewBranchPath viewed bpath

        ListChildPath left bpath right ->
            -- TODO: This is not right, we've no idea how to separate these.
            -- I'll come back to this when I understand more what we're doing.
            -- I think we probably need some kind of typing to say something about
            -- a list child path, ie. what kind of node we're under.
            Element.column
                -- The unusual spacing is so that I see where this is happening and understand what I have to do.
                [ Element.spacing 20 ]
                (mapUpList viewTerm left (viewBranchPath viewed bpath) right)


viewBranchPath : Element msg -> BranchPath JsonNode -> Element msg
viewBranchPath viewed bpath =
    let
        children =
            mapUpList viewChild bpath.left viewed bpath.right

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


usualSpacing : Element.Attribute msg
usualSpacing =
    Element.spacing 5
