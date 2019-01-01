module Types exposing
    ( Action
    , ActionId
    , BranchPath
    , Buffer
    , Child(..)
    , EditorState
    , Fragment
    , Location
    , Path(..)
    , Term(..)
    , defaultIsAvailable
    , extractFromChild
    , extractFromChildren
    , mapChild
    , mapUpList
    , upList
    , updateStateLocation
    )

import Dict exposing (Dict)



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


type alias Buffer node =
    { state : EditorState node

    -- This means you just have a set list of actions and they each have an 'isAvailable' function.
    -- Alternatively we could have a function from EditorState node -> List (Action node).
    , actions : Dict ActionId (Action node)
    , keys : Dict String ActionId

    -- The keys which are active when a leaf input is in focus, for example you may have bound the 'hjkl'
    -- keys to the left,up,down,right actions, but you don't want them taking effect whilst in a leaf input.
    , leafKeys : Dict String ActionId
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



-- Typically 'term' will be `Term node`


type Child term
    = Singleton term
    | OptionalChild term
    | ListChild (List term)


type Term node
    = Leaf String
    | Branch node (List (Child (Term node)))


type Path node
    = Top
    | SingleChildPath (BranchPath node)
    | OptionalChildPath (BranchPath node)
    | ListChildPath (List (Term node)) (BranchPath node) (List (Term node))


type alias BranchPath node =
    { kind : node
    , left : List (Child (Term node))
    , up : Path node
    , right : List (Child (Term node))
    }


extractFromChild : Child a -> List a
extractFromChild child =
    case child of
        Singleton a ->
            [ a ]

        OptionalChild a ->
            [ a ]

        ListChild someAs ->
            someAs


extractFromChildren : List (Child a) -> List a
extractFromChildren children =
    List.concat <| List.map extractFromChild children


mapChild : (a -> b) -> Child a -> Child b
mapChild f child =
    case child of
        Singleton a ->
            Singleton <| f a

        OptionalChild a ->
            OptionalChild <| f a

        ListChild someAs ->
            ListChild <| List.map f someAs
