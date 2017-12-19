module CacheTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Dict
import Cache


suite : Test
suite =
    describe "LinkedList tests"
        [ describe "Basics"
            -- Nest as many descriptions as you like.
            [ test "Putting and eviction works" <|
                \_ ->
                    let
                        expected =
                            Dict.empty
                                |> Dict.insert "b" 2
                                |> Dict.insert "c" 3
                                |> Dict.insert "d" 4
                                |> Dict.insert "e" 5

                        actual =
                            Cache.newCache 4
                                |> Cache.put "a" 1
                                |> Cache.put "b" 2
                                |> Cache.put "c" 3
                                |> Cache.put "d" 4
                                |> Cache.put "e" 5
                                |> Cache.toDict
                    in
                        Expect.equalDicts expected actual
            , test "Getting works" <|
                \_ ->
                    let
                        expected =
                            Just 2

                        ( _, actual ) =
                            Cache.newCache 4
                                |> Cache.put "a" 1
                                |> Cache.put "b" 2
                                |> Cache.put "c" 3
                                |> Cache.put "d" 4
                                |> Cache.put "e" 5
                                |> Cache.get "b"
                    in
                        Expect.equal expected actual
            , test "Getting Nothing when key has been evicted" <|
                \_ ->
                    let
                        expected =
                            Nothing

                        ( _, actual ) =
                            Cache.newCache 2
                                |> Cache.put "a" 1
                                |> Cache.put "b" 2
                                |> Cache.put "c" 3
                                |> Cache.get "a"
                    in
                        Expect.equal expected actual
            , test "Multiple evictions work" <|
                \_ ->
                    let
                        expected =
                            Dict.empty
                                |> Dict.insert "d" 4
                                |> Dict.insert "e" 5

                        actual =
                            Cache.newCache 2
                                |> Cache.put "a" 1
                                |> Cache.put "b" 2
                                |> Cache.put "c" 3
                                |> Cache.put "d" 4
                                |> Cache.put "e" 5
                                |> Cache.toDict
                    in
                        Expect.equalDicts expected actual
            , test "Get bumps to the front and changes lru" <|
                \_ ->
                    let
                        expected =
                            Dict.empty
                                |> Dict.insert "a" 1
                                |> Dict.insert "d" 4
                                |> Dict.insert "e" 5

                        actual =
                            Cache.newCache 3
                                |> Cache.put "a" 1
                                |> Cache.put "b" 2
                                |> Cache.put "c" 3
                                |> Cache.get "a"
                                |> Tuple.first
                                |> Cache.put "d" 4
                                |> Cache.put "e" 5
                                |> Cache.toDict
                    in
                        Expect.equalDicts expected actual
            , test "Putting the same thing takes expected space" <|
                \_ ->
                    let
                        expected =
                            Dict.empty
                                |> Dict.insert "a" 1
                                |> Dict.insert "b" 2

                        actual =
                            Cache.newCache 3
                                |> Cache.put "a" 1
                                |> Cache.put "a" 1
                                |> Cache.put "a" 1
                                |> Cache.put "a" 1
                                |> Cache.put "a" 1
                                |> Cache.put "b" 2
                                |> Cache.toDict
                    in
                        Expect.equalDicts expected actual
            , test "Remove an item from the cache" <|
                \_ ->
                    let
                        expected =
                            Dict.empty
                                |> Dict.insert "a" 1
                                |> Dict.insert "c" 3

                        actual =
                            Cache.newCache 3
                                |> Cache.put "a" 1
                                |> Cache.put "b" 2
                                |> Cache.put "c" 3
                                |> Cache.remove "b"
                                |> Cache.toDict
                    in
                        Expect.equalDicts expected actual
            , test "Remove all items" <|
                \_ ->
                    let
                        expected =
                            Dict.empty
                                |> Dict.insert "a" 1

                        actual =
                            Cache.newCache 3
                                |> Cache.put "a" 1
                                |> Cache.put "b" 2
                                |> Cache.put "c" 3
                                |> Cache.remove "b"
                                |> Cache.remove "c"
                                |> Cache.remove "a"
                                |> Cache.put "a" 1
                                |> Cache.toDict
                    in
                        Expect.equalDicts expected actual
            , test "Eviction works after deletions" <|
                \_ ->
                    let
                        expected =
                            Dict.empty
                                |> Dict.insert "c" 3
                                |> Dict.insert "d" 4

                        actual =
                            Cache.newCache 2
                                |> Cache.put "a" 1
                                |> Cache.put "b" 2
                                |> Cache.put "c" 3
                                |> Cache.remove "a"
                                |> Cache.put "d" 4
                                |> Cache.toDict
                    in
                        Expect.equalDicts expected actual
            ]
        ]
