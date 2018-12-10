module ElmMode exposing
    ( Node(..)
    , newBuffer
    )

import GenericActions
import Types
    exposing
        ( Buffer
        , Location
        )


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
