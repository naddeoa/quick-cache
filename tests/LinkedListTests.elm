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

                        actual = LinkedList.toList actualLinkedList

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
            ]
        ]
