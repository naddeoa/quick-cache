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


linkPrevious : Link -> Maybe String
linkPrevious (Link { previous }) =
    previous


has : String -> LinkedList -> Bool
has a (LinkedList { nodes }) =
    Dict.member a nodes


type LinkedListError
    = NodeNotInList


isEmpty : LinkedList -> Bool
isEmpty list =
    size list == 0


first : LinkedList -> Maybe String
first (LinkedList { firstAndLast }) =
    case firstAndLast of
        Nothing ->
            Nothing

        Just ( first, _ ) ->
            Just first


unsafeLast : LinkedList -> String
unsafeLast (LinkedList { firstAndLast }) =
    case firstAndLast of
        Nothing ->
            "~~NOTHING~~"

        Just ( _ , last ) ->
            last


last : LinkedList -> Maybe String
last (LinkedList { firstAndLast }) =
    case firstAndLast of
        Nothing ->
            Nothing

        Just ( last, _ ) ->
            Just last


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


removeLast : LinkedList -> LinkedList
removeLast ((LinkedList { firstAndLast }) as list) =
    case firstAndLast of
        Nothing ->
            list

        Just ( _, last ) ->
            remove last list


remove : String -> LinkedList -> LinkedList
remove a ((LinkedList { firstAndLast, nodes, size }) as list) =
    case firstAndLast of
        Nothing ->
            list

        Just ( first, last ) ->
            let
                linkMaybe =
                    Dict.get a nodes
            in
                case linkMaybe of
                    Nothing ->
                        -- That key wasn't even in this list
                        list

                    Just link ->
                        -- The key is in the list
                        case linkPrevious link of
                            Nothing ->
                                -- If there was no previous link to this item...
                                case linkNext link of
                                    Nothing ->
                                        -- AND no next link, it must have been the only thing in the list
                                        LinkedList { firstAndLast = Nothing, size = 0, nodes = Dict.empty }

                                    Just nextLinkKey ->
                                        let
                                            nextLink =
                                                unsafeGetLink nextLinkKey list
                                        in
                                            -- BUT it has a next, it must have been the head. Leave the last alone, set
                                            -- that next link to the new head.
                                            LinkedList
                                                { firstAndLast = Just ( nextLinkKey, last )
                                                , size = size - 1
                                                , nodes =
                                                    nodes
                                                        |> Dict.remove a
                                                        |> Dict.insert nextLinkKey (Link { previous = Nothing, next = linkNext nextLink, value = linkValue nextLink })
                                                }

                            Just previousLinkKey ->
                                -- If there is a previous link...
                                case linkNext link of
                                    Nothing ->
                                        -- AND there is no next, then this must be the last item
                                        let
                                            previousLink =
                                                unsafeGetLink previousLinkKey list
                                        in
                                            LinkedList
                                                { firstAndLast = Just ( first, previousLinkKey )
                                                , size = size - 1
                                                , nodes =
                                                    nodes
                                                        |> Dict.insert previousLinkKey (Link { previous = linkPrevious previousLink, next = Nothing, value = previousLinkKey })
                                                        |> Dict.remove a
                                                }

                                    Just nextLinkKey ->
                                        let
                                            nextLink =
                                                unsafeGetLink nextLinkKey list

                                            previousLink =
                                                unsafeGetLink previousLinkKey list
                                        in
                                            -- AND there is a next, then this item has two siblings and they need to point to each other.
                                            -- It must not have been the first or last item
                                            LinkedList
                                                { firstAndLast = Just ( first, last )
                                                , size = size - 1
                                                , nodes =
                                                    nodes
                                                        |> Dict.insert previousLinkKey (Link { previous = linkPrevious previousLink, next = Just nextLinkKey, value = previousLinkKey })
                                                        |> Dict.insert nextLinkKey (Link { previous = Just previousLinkKey, next = linkNext nextLink, value = nextLinkKey })
                                                        |> Dict.remove a
                                                }


push : String -> LinkedList -> LinkedList
push a list =
    let
        -- If a is already in the list then remove it
        ((LinkedList { firstAndLast, nodes, size }) as listToUse) =
            if has a list then
                (remove a list)
            else
                list
    in
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
                        unsafeGetLink first listToUse

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
