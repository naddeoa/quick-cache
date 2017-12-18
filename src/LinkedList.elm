module LinkedList exposing (..)

import Dict exposing (Dict)


type LinkedList
    = LinkedList
        { firstAndLast : Maybe ( String, String )
        , size : Int
        , nodes : Dict String Link
        }


type Link
    = Link
        { previous : Maybe String
        , next : Maybe String
        , value : String
        }


emptyLink : Link
emptyLink =
    Link { previous = Nothing, next = Nothing, value = "NOTHING" }


linkValue : Link -> String
linkValue (Link { value }) =
    value


linkNext : Link -> Maybe String
linkNext (Link { next }) =
    next


type LinkedListError
    = NodeNotInList


isEmpty : LinkedList -> Bool
isEmpty list =
    size list == 0


size : LinkedList -> Int
size (LinkedList { size }) =
    size


nodes : LinkedList -> Dict String Link
nodes (LinkedList { nodes }) =
    nodes


empty : LinkedList
empty =
    LinkedList { firstAndLast = Nothing, size = 0, nodes = Dict.empty }


unsafeGetLink : String -> LinkedList -> Link
unsafeGetLink a (LinkedList { nodes }) =
    Maybe.withDefault emptyLink <| Dict.get a nodes


toList : LinkedList -> List String
toList ((LinkedList { firstAndLast, size, nodes }) as list) =
    case firstAndLast of
        Nothing ->
            []

        Just ( first, last ) ->
            toListHelper first list []


toListHelper : String -> LinkedList -> List String -> List String
toListHelper currentKey ((LinkedList { nodes }) as list) accumulation =
    let
        (Link { previous, next }) =
            unsafeGetLink currentKey list

        newAccumulation =
            List.append accumulation [ currentKey ]
    in
        case next of
            Nothing ->
                newAccumulation

            Just value ->
                toListHelper value list newAccumulation


push : String -> LinkedList -> LinkedList
push a ((LinkedList { firstAndLast, nodes, size }) as list) =
    case firstAndLast of
        Nothing ->
            -- Nothing is in the list at all. Set this new item to the head, last and
            -- add an entry for it in the nodes map to denote it has no siblings.
            LinkedList
                { firstAndLast = Just ( a, a )
                , size = size + 1
                , nodes = Dict.insert a (Link { previous = Nothing, next = Nothing, value = a }) nodes
                }

        Just ( first, last ) ->
            let
                originalFirstLink =
                    unsafeGetLink first list

                newLink =
                    (Link { previous = Nothing, next = Just (linkValue originalFirstLink), value = a })
            in
                LinkedList
                    { firstAndLast = Just ( a, last )
                    , size = size + 1
                    , nodes =
                        nodes
                            |> Dict.insert a newLink
                            |> Dict.insert (linkValue originalFirstLink) (Link { previous = Just (linkValue newLink), next = linkNext originalFirstLink, value = a })
                    }



--list
-- We have at least a head node
--    case size of
--        0 ->
--            LinkedList
--                { head = Just a
--                , last = Just a
--                , size = 1
--                , nodes = Dict.insert a (Link { previous = Nothing, next = Nothing, value = a }) nodes
--                }
--
--        1 ->
--            let
--                currentHeadLink =
--                    Maybe.withDefault emptyLink (Dict.get head nodes)
--            in
--                LinkedList
--                    { head = Just a
--                    , last = last
--                    , size = 2
--                    , nodes = Dict.insert a (Link { previous = Nothing, next = currentHeadLink, value = a }) nodes
--                    }


next : LinkedList -> String -> Result LinkedListError (Maybe String)
next (LinkedList { nodes }) a =
    case Dict.get a nodes of
        Just (Link { next }) ->
            case next of
                Nothing ->
                    Ok Nothing

                Just value ->
                    Ok <| Just value

        Nothing ->
            Err NodeNotInList
