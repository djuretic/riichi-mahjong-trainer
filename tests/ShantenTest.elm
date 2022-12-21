module ShantenTest exposing (..)

import Expect
import Shanten
import Test exposing (..)
import Tile


suite : Test
suite =
    describe "Shanten module"
        [ describe "Shanten for Kokushi"
            [ testShantenKokushi "13-sided wait" 0 "19m19p19s1234567z"
            , testShantenKokushi "with one pair" 0 "19m119p19s134567z"
            , testShantenKokushi "with two pairs" 1 "19m1199p19s14567z"
            , testShantenKokushi "only one terminal" 12 "1m2223334445556s"
            , testShantenKokushi "14 tiles, yakuman" -1 "19m19p19s12334567z"
            ]
        , describe "Shanten for Chiitoitsu"
            [ testShantenChiitoitsu "6 pairs" 0 "225588m11p88s223z"
            , testShantenChiitoitsu "2 equal pairs" 1 "222288m11p88s223z"
            , testShantenChiitoitsu "14 tiles, 7 pairs" -1 "225588m11p88s2233z"
            ]
        , describe "Standard shanten (5 groups + pair)"
            [ testShantenStandard "2-shanten" 2 "46789m55779p457s"
            , testShantenStandard "tenpai" 0 "456m567p12388s77z"
            , testShantenStandard "chinitsu" -1 "1112245677889p6p"
            ]
        ]


testShantenKokushi : String -> Int -> String -> Test
testShantenKokushi name shanten hand =
    test name <|
        \_ -> Expect.equal shanten (Shanten.shantenKokushi (Tile.fromString hand) |> .shanten)


testShantenChiitoitsu : String -> Int -> String -> Test
testShantenChiitoitsu name shanten hand =
    test name <|
        \_ -> Expect.equal shanten (Shanten.shantenChiitoitsu (Tile.fromString hand) |> .shanten)


testShantenStandard : String -> Int -> String -> Test
testShantenStandard name shanten hand =
    test name <|
        \_ -> Expect.equal shanten (Shanten.shantenStandard (Tile.fromString hand) |> .shanten)
