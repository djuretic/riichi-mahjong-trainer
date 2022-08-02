module Yaku exposing (..)

import Expect
import Group
import Hand
import Main
import Test exposing (..)


suite : Test
suite =
    describe "Yaku check"
        [ testHandYaku [ Hand.HanSource 1 Hand.Pinfu ] "123678m234p99567s"
        , testHandYaku [ Hand.HanSource 1 Hand.Yakuhai ] "77789m222p456s555z"
        , testHandYaku [ Hand.HanSource 1 Hand.Yakuhai, Hand.HanSource 1 Hand.Yakuhai, Hand.HanSource 2 Hand.Shousangen ] "456p888s55566677z"
        , testHandYaku [ Hand.HanSource 2 Hand.Chanta ] "123999m123s111p44z"
        , testHandYaku [ Hand.HanSource 2 Hand.Toitoi ] "22888m333p111s444z"
        ]


testHandYaku : List Hand.HanSource -> String -> Test
testHandYaku hanSourceList handString =
    test ("hand " ++ handString) <|
        \_ ->
            let
                tiles =
                    Main.showParseResult handString

                allGroups =
                    Group.findGroups2 tiles

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
                        , hanSources = []
                        , fuSources = []
                    }

                handCounted =
                    Hand.count hand
            in
            Expect.equalLists hanSourceList handCounted.hanSources
