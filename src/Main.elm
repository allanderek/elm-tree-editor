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


type alias Fragment =
    Term


type alias Location =
    { current : Term
    , path : Path
    }


type NodeKind
    = Let
    | Apply
    | Decl


type Child kind
    = Singleton kind
    | ListChild (List kind)
    | OptionalChild kind


type Term
    = Leaf String
    | Branch NodeKind (List (Child Term))


type Path
    = Top
    | SingleChildPath BranchPath
    | OptionalChildPath BranchPath
    | ListChildPath (List Term) BranchPath (List Term)


type alias BranchPath =
    { kind : NodeKind
    , left : List (Child Term)
    , up : Path
    , right : List (Child Term)
    }


init : ProgramFlags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        initialModel =
            { key = key
            , url = url
            , editorState =
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
    | EditorAction Action
    | LeafInput String


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
                    case newModel.editorState.location.current of
                        Leaf _ ->
                            focusLeafBox

                        Branch _ _ ->
                            Cmd.none
            in
            withCommands newModel [ command ]

        LeafInput content ->
            let
                newModel =
                    case model.editorState.location.current of
                        Leaf _ ->
                            let
                                editorState =
                                    model.editorState

                                location =
                                    editorState.location

                                newLocation =
                                    { location | current = Leaf content }

                                newState =
                                    { editorState | location = newLocation }
                            in
                            { model | editorState = newState }

                        Branch _ _ ->
                            model
            in
            withCommands newModel []


withCommands : model -> List (Cmd msg) -> ( model, Cmd msg )
withCommands model commands =
    ( model, Cmd.batch commands )


goLeft : Action
goLeft =
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
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


goRight : Action
goRight =
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
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


goUp : Action
goUp =
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
    { updateState = updateState
    , isAvailable = defaultIsAvailable updateState
    }


goDown : Action
goDown =
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
    { updateState = updateState
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
        [ makeButton goLeft "Left"
        , makeButton goUp "Up"
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


idAttribute : String -> Element.Attribute msg
idAttribute s =
    Element.htmlAttribute <| Attributes.id s


classAttribute : String -> Element.Attribute msg
classAttribute s =
    Element.htmlAttribute <| Attributes.class s
