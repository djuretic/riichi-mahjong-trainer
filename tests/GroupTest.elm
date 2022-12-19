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
        , test "findGroupsInSuits, partial group consecutive tiles" <|
            \_ ->
                Expect.equalLists
                    [ [ Group PartialRyanmenPenchan 1 Man, Group Triplet 7 Man ] ]
                    (Group.findGroupsInSuit FindPartials Man (Tile.fromString "12777m"))
        , test "findGroupsInSuits, partial group consecutive tiles 2" <|
            \_ ->
                Expect.equalLists
                    [ [ Group Triplet 1 Man, Group PartialRyanmenPenchan 7 Man ] ]
                    (Group.findGroupsInSuit FindPartials Man (Tile.fromString "11178m"))
        , test "findGroupsInSuits, partial group kanchan" <|
            \_ ->
                Expect.equalLists
                    [ [ Group PartialKanchan 1 Man, Group Run 7 Man ] ]
                    (Group.findGroupsInSuit FindPartials Man (Tile.fromString "13789m"))
        ]
