module Shanten exposing (shantenChiitoitsu, shantenKokushi, shantenStandard)

import Group
import List.Extra
import Suit
import Tile exposing (Tile)


shantenKokushi : List Tile -> Int
shantenKokushi tiles =
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


shantenChiitoitsu : List Tile -> Int
shantenChiitoitsu tiles =
    let
        pairs =
            findPairs tiles
                |> Tile.deduplicate
    in
    6 - List.length pairs


findPairs : List Tile -> List Group.Group
findPairs tiles =
    case tiles of
        [] ->
            []

        x :: y :: xs ->
            if x == y then
                Group.pairOf x :: findPairs xs

            else
                findPairs (y :: xs)

        _ :: xs ->
            findPairs xs


shantenStandard : List Tile -> Int
shantenStandard tiles =
    let
        highestShantenScore =
            \lg ->
                List.sortBy (\g -> Group.completionScore g) lg
                    |> List.reverse
                    |> List.filterMap List.head

        groups =
            Group.findGroups Group.FindPartials tiles
                |> Group.breakdownConcatMap highestShantenScore

        ( scoreCompleteGroups, scorePartialGroups ) =
            Group.completionScore groups
    in
    8 - 2 * scoreCompleteGroups - scorePartialGroups
