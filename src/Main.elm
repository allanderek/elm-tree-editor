module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import BufferMessage exposing (BufferMsg)
import Dict exposing (Dict)
import Element
import ElmMode as Elm
import Html.Attributes as Attributes
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import JsonMode as Json
import List.Extra
import Task
import Types
import Url
import ViewUtils


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


type alias KeyEvent =
    { key : String
    , ctrl : Bool
    , alt : Bool
    }


keySubscription : Sub Msg
keySubscription =
    let
        keyDecoder =
            Decode.succeed KeyEvent
                |> Pipeline.required "key" Decode.string
                |> Pipeline.required "ctrlKey" Decode.bool
                |> Pipeline.required "altKey" Decode.bool

        decoder =
            Decode.map KeyPressed keyDecoder
    in
    Browser.Events.onKeyPress decoder


type alias ProgramFlags =
    ()


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , buffers : List BufferInMode
    , currentBuffer : BufferInMode
    }


type BufferInMode
    = ElmBuffer (Types.Buffer Elm.Node)
    | JsonBuffer (Types.Buffer Json.Node)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOp
    | KeyPressed KeyEvent
    | SelectCurrentBuffer BufferInMode
    | BufferMsg BufferMsg


init : ProgramFlags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        elmBuffer =
            Elm.newBuffer
                { current = Types.Branch Elm.Apply [ Types.ListChild [ Types.Leaf "f", Types.Leaf "a" ] ]
                , path =
                    Types.SingleChildPath
                        { up = Types.Top
                        , kind = Elm.Let
                        , left = [ Types.ListChild [ Types.Branch Elm.Decl [ Types.Singleton <| Types.Leaf "a", Types.Singleton <| Types.Leaf "1" ] ] ]
                        , right = []
                        }
                }

        jsonBuffer =
            Json.newBuffer
                { current =
                    Types.Branch
                        Json.ObjectNode
                        [ Types.ListChild
                            [ Types.Branch Json.FieldNode [ Types.Singleton <| Types.Leaf "first", Types.Singleton <| Types.Leaf "1" ]
                            , Types.Branch Json.FieldNode [ Types.Singleton <| Types.Leaf "second", Types.Singleton <| Types.Leaf "2" ]
                            , Types.Branch Json.FieldNode [ Types.Singleton <| Types.Leaf "third", Types.Singleton <| Types.Leaf "3" ]
                            ]
                        ]
                , path = Types.Top
                }

        initialModel =
            { key = key
            , url = url
            , buffers = [ ElmBuffer elmBuffer ]
            , currentBuffer = JsonBuffer jsonBuffer
            }
    in
    withCommands initialModel []


focusLeafBox : Cmd Msg
focusLeafBox =
    Task.attempt (always NoOp) (Browser.Dom.focus ViewUtils.leafBoxId)


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

        KeyPressed event ->
            case event.key == "w" && event.alt of
                True ->
                    case model.buffers of
                        [] ->
                            withCommands model []

                        first :: rest ->
                            let
                                newModel =
                                    { model
                                        | currentBuffer = first
                                        , buffers = rest ++ [ model.currentBuffer ]
                                    }
                            in
                            withCommands newModel []

                False ->
                    case model.currentBuffer of
                        ElmBuffer buffer ->
                            let
                                ( newBuffer, command ) =
                                    applyKey event buffer
                            in
                            withCommands { model | currentBuffer = ElmBuffer newBuffer } [ command ]

                        JsonBuffer buffer ->
                            let
                                ( newBuffer, command ) =
                                    applyKey event buffer
                            in
                            withCommands { model | currentBuffer = JsonBuffer newBuffer } [ command ]

        SelectCurrentBuffer newCurrentBuffer ->
            let
                newModel =
                    { model
                        | currentBuffer = newCurrentBuffer
                        , buffers = List.Extra.remove newCurrentBuffer model.buffers
                    }
            in
            withCommands newModel []

        BufferMsg bufferMessage ->
            case model.currentBuffer of
                ElmBuffer buffer ->
                    let
                        ( newBuffer, command ) =
                            updateBuffer bufferMessage buffer
                    in
                    withCommands { model | currentBuffer = ElmBuffer newBuffer } [ command ]

                JsonBuffer buffer ->
                    let
                        ( newBuffer, command ) =
                            updateBuffer bufferMessage buffer
                    in
                    withCommands { model | currentBuffer = JsonBuffer newBuffer } [ command ]


updateBuffer : BufferMsg -> Types.Buffer node -> ( Types.Buffer node, Cmd Msg )
updateBuffer message buffer =
    case message of
        BufferMessage.EditorAction actionId ->
            applyAction actionId buffer

        BufferMessage.LeafInput content ->
            let
                newBuffer =
                    case buffer.state.location.current of
                        Types.Leaf _ ->
                            let
                                state =
                                    buffer.state

                                location =
                                    buffer.state.location

                                newLocation =
                                    { location | current = Types.Leaf content }

                                newState =
                                    { state | location = newLocation }
                            in
                            { buffer | state = newState }

                        Types.Branch _ _ ->
                            buffer
            in
            withCommands newBuffer []


applyKey : KeyEvent -> Types.Buffer node -> ( Types.Buffer node, Cmd Msg )
applyKey event buffer =
    case Dict.get event.key buffer.keys of
        Nothing ->
            withCommands buffer []

        Just actionId ->
            applyAction actionId buffer


applyAction : Types.ActionId -> Types.Buffer node -> ( Types.Buffer node, Cmd Msg )
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
                        Types.Leaf _ ->
                            focusLeafBox

                        Types.Branch _ _ ->
                            Cmd.none
            in
            withCommands newBuffer [ command ]


withCommands : model -> List (Cmd msg) -> ( model, Cmd msg )
withCommands model commands =
    ( model, Cmd.batch commands )


view : Model -> Browser.Document Msg
view model =
    let
        document =
            case model.currentBuffer of
                ElmBuffer buffer ->
                    Elm.view buffer

                JsonBuffer buffer ->
                    Json.view buffer

        makeBufferControl newCurrentBuffer =
            let
                msg =
                    case model.currentBuffer == newCurrentBuffer of
                        True ->
                            Nothing

                        False ->
                            Just <| SelectCurrentBuffer newCurrentBuffer

                title =
                    case newCurrentBuffer of
                        ElmBuffer _ ->
                            "Elm"

                        JsonBuffer _ ->
                            "Json"
            in
            ViewUtils.viewButton title msg

        bufferControls =
            Element.row
                []
                (List.map makeBufferControl model.buffers)

        body =
            Element.column
                []
                [ bufferControls
                , Element.map BufferMsg document.body
                ]
    in
    { title = document.title
    , body = [ Element.layout [] body ]
    }
