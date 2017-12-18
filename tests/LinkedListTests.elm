module LinkedListTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict
import LinkedList


suite : Test
suite =
    describe "LinkedList tests"
        [ describe "Basics"
            -- Nest as many descriptions as you like.
            [ test "Pushing works" <|
                \_ ->
                    let
                        expected =
                            [ "c", "b", "a" ]

                        actualLinkedList =
                            LinkedList.empty
                                |> LinkedList.push "a"
                                |> LinkedList.push "b"
                                |> LinkedList.push "c"

                        actual =
                            LinkedList.toList actualLinkedList
                    in
                        Expect.equalLists expected actual
            , test "Size counted correctly" <|
                \_ ->
                    let
                        expected =
                            3

                        actual =
                            LinkedList.empty
                                |> LinkedList.push "a"
                                |> LinkedList.push "b"
                                |> LinkedList.push "c"
                    in
                        Expect.equal expected (Dict.size (LinkedList.nodes actual))
            , test "Remove first works" <|
                \_ ->
                    let
                        expected =
                            [ "b", "a" ]

                        actualLinkedList =
                            LinkedList.empty
                                |> LinkedList.push "a"
                                |> LinkedList.push "b"
                                |> LinkedList.push "c"
                                |> LinkedList.remove "c"

                        actual =
                            LinkedList.toList actualLinkedList
                    in
                        Expect.equal expected actual
            , test "Remove last works" <|
                \_ ->
                    let
                        expected =
                            [ "c", "b" ]

                        actualLinkedList =
                            LinkedList.empty
                                |> LinkedList.push "a"
                                |> LinkedList.push "b"
                                |> LinkedList.push "c"
                                |> LinkedList.remove "a"

                        actual =
                            LinkedList.toList actualLinkedList
                    in
                        Expect.equal expected actual
            , test "Remove middle works" <|
                \_ ->
                    let
                        expected =
                            [ "c", "a" ]

                        actualLinkedList =
                            LinkedList.empty
                                |> LinkedList.push "a"
                                |> LinkedList.push "b"
                                |> LinkedList.push "c"
                                |> LinkedList.remove "b"

                        actual =
                            LinkedList.toList actualLinkedList
                    in
                        Expect.equal expected actual
            , test "Remove single item list" <|
                \_ ->
                    let
                        expected =
                            []

                        actualLinkedList =
                            LinkedList.empty
                                |> LinkedList.push "a"
                                |> LinkedList.remove "a"

                        actual =
                            LinkedList.toList actualLinkedList
                    in
                        Expect.equal expected actual
            , test "Remove and add back single item list" <|
                \_ ->
                    let
                        expected =
                            [ "b", "a" ]

                        actualLinkedList =
                            LinkedList.empty
                                |> LinkedList.push "a"
                                |> LinkedList.remove "a"
                                |> LinkedList.push "a"
                                |> LinkedList.push "b"

                        actual =
                            LinkedList.toList actualLinkedList
                    in
                        Expect.equal expected actual
            , test "Remove items that don't exit" <|
                \_ ->
                    let
                        expected =
                            [ "b", "c" ]

                        actualLinkedList =
                            LinkedList.empty
                                |> LinkedList.push "c"
                                |> LinkedList.push "b"
                                |> LinkedList.remove "a"
                                |> LinkedList.remove "a"
                                |> LinkedList.remove "a"
                                |> LinkedList.remove "a"

                        actual =
                            LinkedList.toList actualLinkedList
                    in
                        Expect.equal expected actual
            , test "Adding an item already in the list just brings it to the front" <|
                -- TODO add a test for a very large number of elements
                \_ ->
                    let
                        expected =
                            [ "a", "d", "c", "b" ]

                        actualLinkedList =
                            LinkedList.empty
                                |> LinkedList.push "a"
                                |> LinkedList.push "b"
                                |> LinkedList.push "c"
                                |> LinkedList.push "d"
                                |> LinkedList.push "a"

                        actual =
                            LinkedList.toList actualLinkedList
                    in
                        Expect.equal expected actual
            ]
        ]
