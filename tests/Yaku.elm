module Yaku exposing (..)

import Expect
import Group
import Hand
import Test exposing (..)
import Tile


suite : Test
suite =
    describe "Yaku check"
        [ testHandYaku [ Hand.HanSource 1 Hand.Pinfu ] "123678m234p99567s"
        , testHandYaku [ Hand.HanSource 1 Hand.Yakuhai ] "77789m222p456s555z"
        , testHandYaku [ Hand.HanSource 1 Hand.Yakuhai, Hand.HanSource 1 Hand.Yakuhai, Hand.HanSource 2 Hand.Shousangen ] "456p789s55566677z"
        , testHandYaku [ Hand.HanSource 2 Hand.Chanta ] "123999m123s111p44z"
        , testHandYaku [ Hand.HanSource 2 Hand.Toitoi, Hand.HanSource 2 Hand.Sanankou ] "22888m333p111s444z"
        , testHandYaku [ Hand.HanSource 13 Hand.Suuankou ] "2888m333p111s444z2m"
        , testHandYaku [ Hand.HanSource 2 Hand.Chiitoitsu ] "1122334455m1122p"
        ]


testHandYaku : List Hand.HanSource -> String -> Test
testHandYaku hanSourceList handString =
    test ("hand " ++ handString) <|
        \_ ->
            let
                tiles =
                    Tile.fromString handString

                allGroups =
                    Group.findGroups Group.SkipPartials tiles

                groups =
                    Group.findWinningGroups allGroups

                prevHand =
                    Hand.init

                hand =
                    -- keep the older winds
                    { prevHand
                        | tiles = tiles
                        , winBy = Hand.Ron
                        , groups = groups
                        , hanSources = []
                        , fuSources = []
                    }

                handCounted =
                    Hand.count hand
            in
            Expect.equalLists hanSourceList handCounted.hanSources
