module ViewUtils exposing
    ( body
    , errorColor
    , header
    , indentElement
    , indentation
    , leafBoxId
    , themeColor1
    , themeColor2
    , themeColor3
    , themeColor4
    , themeColor5
    , usualSpacing
    , viewButton
    , viewErrored
    , viewFocusedLeaf
    , viewHighlighted
    , viewKeyword
    , viewPunctuation
    )

import BufferMessage exposing (BufferMsg(..))
import Dict
import Element exposing (Element, el, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Types


body : Types.Buffer node -> Element BufferMsg -> Element BufferMsg -> Element BufferMsg
body buffer clipBoard mainContent =
    let
        controls =
            Element.column
                []
                [ header buffer
                , clipBoard
                ]
    in
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


viewButton : String -> Maybe msg -> Element msg
viewButton title mMsg =
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


header : Types.Buffer node -> Element BufferMsg
header buffer =
    let
        makeButton action =
            case action.isAvailable buffer.state of
                True ->
                    viewButton action.name <| Just (EditorAction action.actionId)

                False ->
                    viewButton action.name Nothing
    in
    Element.wrappedRow
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.spaceEvenly
        ]
        (List.map makeButton <| Dict.values buffer.actions)


usualSpacing : Element.Attribute msg
usualSpacing =
    Element.spacing 5


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


indentElement : Element msg -> Element msg
indentElement block =
    Element.row [] [ indentation, block ]


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


viewFocusedLeaf : String -> Element BufferMsg
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


leafBoxId : String
leafBoxId =
    "leaf-box"


idAttribute : String -> Element.Attribute msg
idAttribute s =
    Element.htmlAttribute <| Html.Attributes.id s


classAttribute : String -> Element.Attribute msg
classAttribute s =
    Element.htmlAttribute <| Html.Attributes.class s



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
