module Shanten exposing (shantenChiitoitsu, shantenKokushi, shantenStandard)

import Group exposing (Group)
import List
import List.Extra
import Suit
import Tile exposing (Tile)


type alias ShantenDetail =
    { kokushi : ShantenCalculation
    , chiitoitsu : ShantenCalculation
    , standard : ShantenCalculation
    , shanten : Int
    }


type alias ShantenCalculation =
    { shanten : Int
    , groups : List Group
    }


shanten : List Tile -> ShantenDetail
shanten tiles =
    let
        kokushi =
            shantenKokushi tiles

        chiitoitsu =
            shantenChiitoitsu tiles

        standard =
            shantenStandard tiles
    in
    { kokushi = kokushi
    , chiitoitsu = chiitoitsu
    , standard = standard
    , shanten = min kokushi.shanten chiitoitsu.shanten |> min standard.shanten
    }


shantenKokushi : List Tile -> ShantenCalculation
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
    { shanten =
        List.foldl
            (\( _, count ) ( shantenN, foundPair ) ->
                if count == 1 then
                    ( shantenN - 1, foundPair )

                else if count == 2 then
                    if foundPair then
                        ( shantenN - 1, True )

                    else
                        ( shantenN - 2, True )

                else
                    ( shantenN, foundPair )
            )
            ( 13, False )
            counter
            |> Tuple.first
    , groups = []
    }


shantenChiitoitsu : List Tile -> ShantenCalculation
shantenChiitoitsu tiles =
    let
        pairs =
            findPairs tiles
                |> Tile.deduplicate
    in
    { shanten = 6 - List.length pairs, groups = pairs }


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


shantenStandard : List Tile -> ShantenCalculation
shantenStandard tiles =
    let
        groups =
            Group.findGroups Group.FindPartials tiles
                |> Group.breakdownConcatMap (\g -> List.head g |> Maybe.withDefault [])

        ( scoreCompleteGroups, scorePartialGroups ) =
            Group.completionScore groups
    in
    { shanten = 8 - 2 * scoreCompleteGroups - scorePartialGroups, groups = groups }
