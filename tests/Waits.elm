module Waits exposing (..)

import Expect
import Group
import Test exposing (..)
import Tile


suite : Test
suite =
    describe "Wait"
        [ testWaits "Nakabukure" "4s" "3445s"
        , testWaits "Shanpon" "9p4s" "44s99p"
        , testWaits "Nobetan" "25p" "2345p"
        , testWaits "Sanmentan" "258s" "2345678s"
        , testWaits "Entotsu" "36s4z" "45666s44z"
        , testWaits "Aryanmen" "58m" "6788m"
        , testWaits "Ryanyan" "346p" "4555p"
        , testWaits "Pentan" "13m" "1222m"
        , testWaits "Kantan" "56s" "5777s"
        , testWaits "Kantankan" "456s" "3335777s"
        , testWaits "Tatsumaki" "56789p" "6667888p"
        , testWaits "Happoubijin" "12345678s" "2223456777s"
        ]


testWaits : String -> String -> String -> Test
testWaits name waits hand =
    test name <|
        \_ -> Expect.equalLists (Tile.fromString waits) (Group.winningTiles (Tile.fromString hand) |> List.map Tuple.first)
