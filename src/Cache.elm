module Cache exposing (..)

import Dict exposing (Dict)
import LinkedList exposing (LinkedList)


type Cache a
    = Cache
        { entries : Dict String a
        , maxSize : Int
        , ordering : LinkedList
        }


newCache : Int -> Cache a
newCache maxSize =
    Cache { entries = Dict.empty, maxSize = maxSize, ordering = LinkedList.empty }


toDict : Cache a -> Dict String a
toDict (Cache { entries }) =
    entries


put : String -> a -> Cache a -> Cache a
put key a ((Cache { entries, maxSize, ordering }) as cache) =
    let
        updatedEntries =
            Dict.insert key a entries

        updatedOrdering =
            LinkedList.push key ordering
    in
        if (Dict.size updatedEntries) <= maxSize then
            Cache { entries = updatedEntries, maxSize = maxSize, ordering = updatedOrdering }
        else
            let
                lruKey =
                    (LinkedList.unsafeLast ordering)

                entriesWithoutLru =
                    Dict.remove lruKey updatedEntries

                orderingWithoutLru =
                    LinkedList.remove lruKey updatedOrdering
            in
                Cache { entries = entriesWithoutLru, maxSize = maxSize, ordering = orderingWithoutLru }


get : String -> Cache a -> ( Cache a, Maybe a )
get key ((Cache { entries, maxSize, ordering }) as cache) =
    case Dict.get key entries of
        Nothing ->
            ( cache, Nothing )

        Just entry ->
            ( Cache { entries = entries, maxSize = maxSize, ordering = LinkedList.push key ordering }, Just entry )
