module Shanten exposing
    ( ShantenDetail
    , TileAcceptance(..)
    , init
    , shanten
    , shantenChiitoitsu
    , shantenKokushi
    , shantenStandard
    , tileAcceptance
    )

import Group exposing (Group)
import List
import List.Extra
import Suit
import Tile exposing (Tile)


type alias ShantenDetail =
    { kokushi : ShantenCalculation
    , chiitoitsu : ShantenCalculation
    , standard : ShantenCalculation
    , final : ShantenCalculation
    }


type alias ShantenCalculation =
    { shanten : Int
    , groups : List (List Group)
    }


type TileAcceptance
    = DiscardAndDraw (List ( Tile, List Tile ))
    | Draw (List Tile)


shanten : List Tile -> ShantenDetail
shanten tiles =
    let
        kokushi =
            shantenKokushi tiles

        chiitoitsu =
            shantenChiitoitsu tiles

        standard =
            shantenStandard tiles

        minShanten =
            List.map .shanten [ kokushi, chiitoitsu, standard ]
                |> List.minimum
                |> Maybe.withDefault 8

        groups =
            List.filter (\sh -> sh.shanten == minShanten) [ kokushi, chiitoitsu, standard ]
                |> List.map .groups
                |> List.concat
    in
    { kokushi = kokushi
    , chiitoitsu = chiitoitsu
    , standard = standard
    , final = { shanten = minShanten, groups = groups }
    }


init : ShantenDetail
init =
    let
        base =
            { shanten = 8, groups = [] }
    in
    { kokushi = base
    , chiitoitsu = base
    , standard = base
    , final = base
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
            Tile.sort tiles
                |> findPairs
                |> Tile.deduplicate
    in
    { shanten = 6 - List.length pairs, groups = [ pairs ] }


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


expectedGroups : Int -> Int
expectedGroups numTiles =
    case numTiles of
        4 ->
            2

        5 ->
            2

        7 ->
            3

        8 ->
            3

        10 ->
            4

        11 ->
            4

        _ ->
            5


shantenStandard : List Tile -> ShantenCalculation
shantenStandard tiles =
    let
        groupConfigurations =
            Group.findGroups Group.FindPartials tiles
                |> Group.breakdownCartesianProduct

        -- TODO are the scores different in some configurations?
        completionScore =
            Group.completionScore (List.head groupConfigurations |> Maybe.withDefault [])

        scoreSum =
            completionScore.groups + completionScore.pairs + completionScore.partials

        noPairPenalty =
            if scoreSum == expectedGroups (List.length tiles) && completionScore.pairs == 0 then
                1

            else
                0

        tooManyGroupsPenalty =
            max 0 (scoreSum - 5)

        baselineScore =
            case List.length tiles of
                4 ->
                    2

                5 ->
                    2

                7 ->
                    4

                8 ->
                    4

                10 ->
                    6

                11 ->
                    6

                _ ->
                    8

        -- _ = Debug.log "aaa" { baseLineScore = baselineScore, completionScore = completionScore, noPairPenalty = noPairPenalty, tooManyGroupsPenalty = tooManyGroupsPenalty  }
    in
    { shanten = baselineScore - 2 * completionScore.groups - completionScore.pairs - completionScore.partials + noPairPenalty + tooManyGroupsPenalty
    , groups = groupConfigurations
    }


tileAcceptance : List Tile -> TileAcceptance
tileAcceptance tiles =
    let
        currentShanten =
            shanten tiles

        numTiles =
            List.length tiles
    in
    if currentShanten.final.shanten >= 0 then
        if List.member numTiles [ 4, 7, 10, 13 ] then
            Draw (drawnTileAcceptance currentShanten.final.shanten tiles)

        else if List.member numTiles [ 5, 8, 11, 14 ] then
            let
                uniqueTiles =
                    List.Extra.unique tiles

                discardsAndAcceptance =
                    List.map (\t -> ( t, drawnTileAcceptance currentShanten.final.shanten (List.Extra.remove t tiles) )) uniqueTiles
                        |> List.filter (\( _, acceptance ) -> not (List.isEmpty acceptance))
            in
            DiscardAndDraw discardsAndAcceptance

        else
            Draw []

    else
        Draw []


drawnTileAcceptance : Int -> List Tile -> List Tile
drawnTileAcceptance baseShanten tiles =
    let
        -- TODO filter tiles already in hand
        tilesToDraw =
            Tile.allTiles

        shantenByTile =
            List.map (\t -> ( t, shanten (t :: tiles) )) tilesToDraw
                |> List.filter (\( _, sd ) -> sd.final.shanten < baseShanten)
    in
    List.map Tuple.first shantenByTile
