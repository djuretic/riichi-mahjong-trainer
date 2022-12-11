module ShantenTest exposing (..)

import Expect
import Shanten
import Test exposing (..)
import Tile


suite : Test
suite =
    describe "Kokushi Shanten"
        [ testKokushiShanten "13-sided wait" 0 "19m19p19s1234567z"
        , testKokushiShanten "with one pair" 0 "19m119p19s134567z"
        , testKokushiShanten "with two pairs" 1 "19m1199p19s14567z"
        , testKokushiShanten "only one terminal" 12 "1m2223334445556s"
        ]


testKokushiShanten : String -> Int -> String -> Test
testKokushiShanten name shanten hand =
    test name <|
        \_ -> Expect.equal shanten (Shanten.kokushiShanten (Tile.fromString hand))
