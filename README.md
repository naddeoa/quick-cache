Elm package: http://package.elm-lang.org/packages/naddeoa/quick-cache/latest

A simple, least recently used (LRU) cache with O(1) get, put and remove operations.

An LRU cache is one that has a fixed size at any point in time and evicts entries
when the size gets over that limit. It will always get rid of the least recently used
entry.

```elm
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
```

Since this library depends on dictionaries internally that require that keys be
comparable you'll have to use `String` as keys for now.

