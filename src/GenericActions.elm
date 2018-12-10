module GenericActions exposing
    ( defaultActions
    , defaultKeys
    , goDown
    , goLeft
    , goRight
    , goUp
    )

import Dict exposing (Dict)
import List.Extra
import Types
    exposing
        ( Action
        , ActionId
        , Child(..)
        , Path(..)
        , Term(..)
        )


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
            [ goLeft
            , goRight
            , goUp
            , goDown
            ]
    in
    List.foldl addAction Dict.empty actions


goLeft : Action node
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
            Types.updateStateLocation updateLocation
    in
    { actionId = "goLeft"
    , name = "Left"
    , updateState = updateState
    , isAvailable = Types.defaultIsAvailable updateState
    }


goRight : Action node
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
            Types.updateStateLocation updateLocation
    in
    { actionId = "goRight"
    , name = "Right"
    , updateState = updateState
    , isAvailable = Types.defaultIsAvailable updateState
    }


goUp : Action node
goUp =
    let
        updateLocation location =
            let
                branchPathUp branchPath current =
                    { path = branchPath.up
                    , current =
                        Branch branchPath.kind <|
                            Types.upList branchPath.left current branchPath.right
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
                            ListChild <| Types.upList left location.current right
                    in
                    branchPathUp branchPath current

        updateState =
            Types.updateStateLocation updateLocation
    in
    { actionId = "goUp"
    , name = "Up"
    , updateState = updateState
    , isAvailable = Types.defaultIsAvailable updateState
    }


goDown : Action node
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
            Types.updateStateLocation updateLocation
    in
    { actionId = "goDown"
    , name = "Down"
    , updateState = updateState
    , isAvailable = Types.defaultIsAvailable updateState
    }
