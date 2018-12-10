module Message exposing (Msg(..))

import Browser
import Types
import Url


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOp
    | KeyPressed String
    | EditorAction Types.ActionId
    | LeafInput String
