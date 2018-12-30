module BufferMessage exposing (BufferMsg(..))

import Types


type BufferMsg
    = EditorAction Types.ActionId
    | LeafInput String
