module Yaku exposing (..)

import Expect
import Hand
import Main
import Test exposing (..)
import Tile


suite : Test
suite =
    describe "Yaku check"
        [ test "test" <|
            \_ ->
                expectHandYaku (Hand.HanSource 1 Hand.Yakuhai) "77789m222p456s555z"
        ]


expectHandYaku : Hand.HanSource -> String -> Expect.Expectation
expectHandYaku hanSource handString =
    let
        tiles =
            Main.showParseResult handString

        allGroups =
            Tile.findGroups tiles

        groups =
            Main.findWinningHand allGroups

        prevHand =
            Hand.init

        hand =
            -- keep the older winds
            { prevHand
                | tiles = tiles
                , winBy = Hand.Tsumo
                , groups = groups
                , han = []
                , fu = []
            }

        allYaku =
            Hand.checkAllYaku hand
    in
    Expect.equalLists [ hanSource ] allYaku
