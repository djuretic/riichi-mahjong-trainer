module GroupTest exposing (..)

import Expect
import Group exposing (Group, GroupType(..))
import Suit exposing (Suit(..))
import Test exposing (..)
import Tile


suite : Test
suite =
    describe "Group"
        [ test "findGroupsInSuit multiple" <|
            \_ ->
                Expect.equalLists
                    [ [ Group Triplet 1 Man, Group Triplet 2 Man, Group Triplet 3 Man ]
                    , [ Group Run 1 Man, Group Run 1 Man, Group Run 1 Man ]
                    ]
                    (Group.findGroupsInSuit Man (Tile.fromString "111222333m"))
        ]
