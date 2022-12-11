module Shanten exposing (kokushiShanten)

import List.Extra
import Suit
import Tile exposing (Tile)


kokushiShanten : List Tile -> Int
kokushiShanten tiles =
    let
        kokushiTiles =
            List.filter (\t -> t.suit == Suit.Honor || Tile.isTerminal t) Tile.allTiles

        counter : List ( Tile, Int )
        counter =
            List.filterMap
                (\t ->
                    if t.suit == Suit.Honor || Tile.isTerminal t then
                        let
                            tileCount =
                                List.Extra.count ((==) t) tiles
                        in
                        Just ( t, tileCount )

                    else
                        Nothing
                )
                kokushiTiles
    in
    List.foldl
        (\( _, count ) ( shanten, foundPair ) ->
            if count == 1 then
                ( shanten - 1, foundPair )

            else if count == 2 then
                if foundPair then
                    ( shanten - 1, True )

                else
                    ( shanten - 2, True )

            else
                ( shanten, foundPair )
        )
        ( 13, False )
        counter
        |> Tuple.first
