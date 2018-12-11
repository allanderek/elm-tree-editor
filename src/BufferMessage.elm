module BufferMessage exposing (BufferMsg(..))

import Browser
import Types
import Url


type BufferMsg
    = EditorAction Types.ActionId
    | LeafInput String
