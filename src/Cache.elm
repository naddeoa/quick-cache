module Cache exposing (..)


type Cache k v
    = Cache
        { key : k
        , value : v
        , usageOrdering : List
        }



--put : Cache a -> a -> Cache a
