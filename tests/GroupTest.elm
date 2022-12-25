module GroupTest exposing (..)

import Expect
import Group exposing (FindPartialsOption(..), Group, GroupType(..))
import Suit exposing (Suit(..))
import Test exposing (..)
import Tile


suite : Test
suite =
    describe "Group"
        [ test "findGroupsInSuit multiple, complete groups" <|
            \_ ->
                Expect.equalLists
                    [ [ Group Triplet 1 Man, Group Triplet 2 Man, Group Triplet 3 Man ]
                    , [ Group Run 1 Man, Group Run 1 Man, Group Run 1 Man ]
                    ]
                    (Group.findGroupsInSuit SkipPartials Man (Tile.fromString "111222333m"))
        , testPartialGroups "consecutive tiles"
            [ [ Group PartialRyanmenPenchan 1 Man, Group Triplet 7 Man ] ]
            "12777m"
        , testPartialGroups "consecutive tiles 2"
            [ [ Group Triplet 1 Man, Group PartialRyanmenPenchan 7 Man ] ]
            "11178m"
        , testPartialGroups "kanchan"
            [ [ Group PartialKanchan 1 Man, Group Run 7 Man ] ]
            "13789m"
        , testPartialGroups "consecutive tiles or kanchan"
            [ [ Group PartialRyanmenPenchan 4 Man ], [ Group PartialKanchan 5 Man ] ]
            "457m"
        , testPartialGroups "run and pair"
            [ [ Group Run 1 Man, Group Pair 8 Man ] ]
            "12388m"
        , testPartialGroups "2-sided wait 4 tiles"
            [ [ Group Run 1 Man ], [ Group Run 2 Man ] ]
            "1234m"
        , testPartialGroups "pair and run"
            [ [ Group Pair 1 Man, Group Run 2 Man ] ]
            "11234m"
        ]


testPartialGroups : String -> List (List Group) -> String -> Test
testPartialGroups name groups hand =
    test name <|
        \_ ->
            Expect.equalLists
                groups
                (Group.findGroupsInSuit FindPartials Man (Tile.fromString hand))
