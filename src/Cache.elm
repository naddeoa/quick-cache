module Cache exposing (newCache, toDict, put, get, remove)

{-| A simple, least recently used (LRU) cache with O(1) get, put and remove operations.

An LRU cache is one that has a fixed size at any point in time and evicts entries
when the size gets over that limit. It will always get rid of the least recently used
entry.

    myCache =
        Cache.newCache 3
            |> Cache.put "a" 1
            |> Cache.put "b" 2
            |> Cache.put "c" 3
            |> Cache.put "d" 4

    Cache.get "a" => (newCacheState, Nothing)
    Cache.get "b" => (newCacheState, Just 2)
    Cache.get "c" => (newCacheState, Just 3)
    Cache.get "d" => (newCacheState, Just 4)

Since this library depends on dictionaries internally that require that keys be
comparable you'll have to use `String` as keys for now.

@docs newCache, toDict, get, put, remove
-}

import Dict exposing (Dict)
import LinkedList exposing (LinkedList)


type Cache a
    = Cache
        { entries : Dict String a
        , maxSize : Int
        , ordering : LinkedList
        }


{-| Create a new empty cache with a maximum size.
-}
newCache : Int -> Cache a
newCache maxSize =
    Cache { entries = Dict.empty, maxSize = maxSize, ordering = LinkedList.empty }


{-| Get the dict that represents this cache.
-}
toDict : Cache a -> Dict String a
toDict (Cache { entries }) =
    entries


{-| Put a new item in the cache.
This may result in another item being evicted if the new size is
greater than the maximum size. If there was a value previously then
it is over written.
-}
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


{-| Get an item out of the cache.
This returns both a new Cache state and a Maybe for the value. Since this is
a least recently used (LRU) cache, anything that is touched will become the
most recently used and the cache's internal state reflects that. Make sure
to use this new reference afterwords and not the original.
-}
get : String -> Cache a -> ( Cache a, Maybe a )
get key ((Cache { entries, maxSize, ordering }) as cache) =
    case Dict.get key entries of
        Nothing ->
            ( cache, Nothing )

        Just entry ->
            ( Cache { entries = entries, maxSize = maxSize, ordering = LinkedList.push key ordering }, Just entry )


{-| Remove an item from the cache.
-}
remove : String -> Cache a -> Cache a
remove key (Cache { entries, maxSize, ordering }) =
    Cache
        { ordering = LinkedList.remove key ordering
        , entries = Dict.remove key entries
        , maxSize = maxSize
        }
