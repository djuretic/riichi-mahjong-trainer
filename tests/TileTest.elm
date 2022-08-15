module TileTest exposing (..)

import Expect
import Test exposing (..)
import Tile exposing (Suit(..), Tile)


suite : Test
suite =
    describe "Tile"
        [ test "hasMoreThan4Tiles false" <|
            \_ -> Expect.false "Expected to be false" (Tile.hasMoreThan4Tiles (List.repeat 4 (Tile 1 Pin)))
        , test "hasMoreThan4Tiles true" <|
            \_ -> Expect.true "Expected to be true" (Tile.hasMoreThan4Tiles (List.repeat 5 (Tile 1 Pin)))
        ]
