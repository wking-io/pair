module PairSpec exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, maybe)
import Pair
import Test exposing (..)


suite : Test
suite =
    describe "The Pair module"
        [ describe "Pair.mapMaybe"
            [ fuzz (maybe int) "Map over Maybe inside of a Pair and return Pair inside of maybe with the new value" <|
                \randomMaybe ->
                    let
                        result =
                            Pair.from 0 randomMaybe
                                |> Pair.mapMaybe ((*) 2)

                        compareResult num pair =
                            (num * 2) == Pair.right pair
                    in
                    Expect.all
                        [ Expect.notEqual Nothing
                        , Expect.equal <| (Maybe.map2 randomMaybe result |> Maybe.withDefault false)
                        ]
            ]
        ]
