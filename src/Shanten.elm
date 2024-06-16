module Shanten exposing
    ( ShantenDetail
    , TileAcceptance(..)
    , TileAcceptanceDetail
    , emptyTileAcceptanceDetail
    , init
    , shanten
    , shantenChiitoitsu
    , shantenKokushi
    , shantenStandard
    , tileAcceptance
    )

import Counter exposing (Counter)
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


type alias GroupConfiguration =
    { groups : List Group
    , shanten : Int
    }


type alias ShantenCalculation =
    { shanten : Int
    , groupConfigurations : List GroupConfiguration
    }


type alias TileAcceptanceDetail =
    { numTiles : Int
    , tiles : List Tile
    }


type TileAcceptance
    = DiscardAndDraw (List ( Tile, TileAcceptanceDetail ))
    | Draw TileAcceptanceDetail


type alias CounterPerSuit =
    { sou : Counter
    , man : Counter
    , pin : Counter
    , honor : Counter
    }


emptyTileAcceptanceDetail : TileAcceptanceDetail
emptyTileAcceptanceDetail =
    { numTiles = 0, tiles = [] }


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
                |> List.map .groupConfigurations
                |> List.concat
    in
    { kokushi = kokushi
    , chiitoitsu = chiitoitsu
    , standard = standard
    , final = { shanten = minShanten, groupConfigurations = groups }
    }


init : ShantenDetail
init =
    let
        base =
            { shanten = 8, groupConfigurations = [] }
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
    , groupConfigurations = []
    }


shantenChiitoitsu : List Tile -> ShantenCalculation
shantenChiitoitsu tiles =
    let
        pairs =
            Tile.sort tiles
                |> findPairs
                |> Tile.deduplicate

        shantenNum =
            6 - List.length pairs
    in
    { shanten = shantenNum, groupConfigurations = [ { groups = pairs, shanten = List.length pairs } ] }


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
                |> List.map (\lg -> { groups = lg, shanten = shantenNumberFromGroups (List.length tiles) lg })
    in
    { shanten = List.map .shanten groupConfigurations |> List.minimum |> Maybe.withDefault 99, groupConfigurations = groupConfigurations }


shantenNumberFromGroups : Int -> List Group -> Int
shantenNumberFromGroups tilesInHand groups =
    let
        completionScore =
            Group.completionScore groups

        scoreSum =
            completionScore.groups + completionScore.pairs + completionScore.partials

        noPairPenalty =
            if scoreSum == expectedGroups tilesInHand && completionScore.pairs == 0 then
                1

            else
                0

        tooManyGroupsPenalty =
            max 0 (scoreSum - expectedGroups tilesInHand)

        baselineScore =
            case tilesInHand of
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

        -- _ = Debug.log "debug" { groups = List.map Group.toString groups,  baseLineScore = baselineScore, completionScore = completionScore, noPairPenalty = noPairPenalty, tooManyGroupsPenalty = tooManyGroupsPenalty }
    in
    baselineScore - 2 * completionScore.groups - completionScore.pairs - completionScore.partials + noPairPenalty + tooManyGroupsPenalty


tileAcceptance : List Tile -> List Tile -> TileAcceptance
tileAcceptance usedTiles tiles =
    let
        currentShanten =
            shanten tiles

        numTiles =
            List.length tiles
    in
    if currentShanten.final.shanten >= 0 then
        if List.member numTiles [ 4, 7, 10, 13 ] then
            Draw (drawnTileAcceptance currentShanten.final.shanten tiles |> addNumTilesToTileAcceptance (tiles ++ usedTiles))

        else if List.member numTiles [ 5, 8, 11, 14 ] then
            let
                uniqueTiles =
                    List.Extra.unique tiles

                discardsAndAcceptance =
                    List.map (\t -> ( t, drawnTileAcceptance currentShanten.final.shanten (List.Extra.remove t tiles) |> addNumTilesToTileAcceptance (t :: tiles ++ usedTiles) )) uniqueTiles
                        |> List.filter (\( _, acceptance ) -> not (List.isEmpty acceptance.tiles))
                        |> List.sortBy (\( _, acceptance ) -> negate acceptance.numTiles)
            in
            DiscardAndDraw discardsAndAcceptance

        else
            Draw emptyTileAcceptanceDetail

    else
        Draw emptyTileAcceptanceDetail


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


addNumTilesToTileAcceptance : List Tile -> List Tile -> TileAcceptanceDetail
addNumTilesToTileAcceptance usedTiles tiles =
    let
        partition =
            Tile.partitionBySuit usedTiles

        counterPerSuit =
            { pin = List.map .number partition.pin |> Counter.fromIntList
            , man = List.map .number partition.man |> Counter.fromIntList
            , sou = List.map .number partition.sou |> Counter.fromIntList
            , honor = List.map .number partition.honor |> Counter.fromIntList
            }

        resultTiles =
            List.map (\t -> ( t, numRemainingTilesOf counterPerSuit t )) tiles
    in
    { numTiles = List.map Tuple.second resultTiles |> List.sum
    , tiles = List.map Tuple.first resultTiles
    }


numRemainingTilesOf : CounterPerSuit -> Tile -> Int
numRemainingTilesOf counterPerSuit tile =
    let
        used =
            case tile.suit of
                Suit.Pin ->
                    Counter.getCount (tile.number - 1) counterPerSuit.pin

                Suit.Man ->
                    Counter.getCount (tile.number - 1) counterPerSuit.man

                Suit.Sou ->
                    Counter.getCount (tile.number - 1) counterPerSuit.sou

                Suit.Honor ->
                    Counter.getCount (tile.number - 1) counterPerSuit.honor
    in
    4 - used
