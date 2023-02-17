module Tile exposing
    ( ComparableTile
    , Tile
    , TileNumber
    , Wind(..)
    , allSuitTiles
    , allTiles
    , deduplicate
    , fromString
    , greenDragonNumber
    , hasMoreThan4Tiles
    , isRun
    , isTerminal
    , isTriplet
    , isValid
    , moveWinningTileToEnd
    , partitionBySuit
    , push
    , randomList
    , randomWind
    , redDragonNumber
    , removeTileAtPosFromArray
    , removeTileAtPosFromList
    , sort
    , toComparable
    , toString
    , whiteDragonNumber
    , windToString
    , windToTileNumber
    )

import Array
import Counter
import Parser exposing ((|.), (|=))
import Random
import Random.List
import Suit exposing (Suit(..))


type Wind
    = East
    | South
    | West
    | North


type alias TileNumber =
    Int


type alias Tile =
    { number : TileNumber
    , suit : Suit
    }


type alias ComparableTile =
    ( String, TileNumber )


isValid : Tile -> Bool
isValid tile =
    if tile.suit == Honor then
        1 <= tile.number && tile.number <= 7

    else
        1 <= tile.number && tile.number <= 9


isTerminal : Tile -> Bool
isTerminal tile =
    tile.suit /= Suit.Honor && (tile.number == 1 || tile.number == 9)


redDragonNumber : TileNumber
redDragonNumber =
    5


greenDragonNumber : TileNumber
greenDragonNumber =
    6


whiteDragonNumber : TileNumber
whiteDragonNumber =
    7


type alias TilesPerSuit =
    { sou : List Tile
    , man : List Tile
    , pin : List Tile
    , honor : List Tile
    }


toComparable : Tile -> ComparableTile
toComparable tile =
    ( Suit.toString tile.suit, tile.number )


partitionBySuit : List Tile -> TilesPerSuit
partitionBySuit tiles =
    let
        ( pin, rest ) =
            List.partition (\t -> t.suit == Pin) tiles

        ( sou, rest2 ) =
            List.partition (\t -> t.suit == Sou) rest

        ( man, rest3 ) =
            List.partition (\t -> t.suit == Man) rest2
    in
    { sou = sou, man = man, pin = pin, honor = rest3 }


isRun : List TileNumber -> Bool
isRun tiles =
    case tiles of
        x :: y :: [ z ] ->
            x + 1 == y && y + 1 == z

        _ ->
            False


isTriplet : List TileNumber -> Bool
isTriplet tiles =
    case tiles of
        x :: y :: [ z ] ->
            x == y && y == z

        _ ->
            False



-- only works on sorted input


deduplicate : List a -> List a
deduplicate list =
    let
        helper accum previousElement remaining =
            case remaining of
                [] ->
                    accum

                first :: rest ->
                    if first == previousElement then
                        helper accum previousElement rest

                    else
                        helper (first :: accum) first rest
    in
    case list of
        [] ->
            []

        x :: xs ->
            x :: helper [] x xs


windToString : Wind -> String
windToString wind =
    case wind of
        East ->
            "East"

        South ->
            "South"

        West ->
            "West"

        North ->
            "North"


windToTileNumber : Wind -> TileNumber
windToTileNumber wind =
    case wind of
        East ->
            1

        South ->
            2

        West ->
            3

        North ->
            4


hasMoreThan4Tiles : List Tile -> Bool
hasMoreThan4Tiles tiles =
    let
        tilesPerSuit =
            partitionBySuit tiles

        suitHasMoreThan4Tiles : List Tile -> Bool
        suitHasMoreThan4Tiles suitTiles =
            List.map .number suitTiles
                |> Counter.fromIntList
                |> Counter.hasCountGreaterThan 4
    in
    suitHasMoreThan4Tiles tilesPerSuit.man
        || suitHasMoreThan4Tiles tilesPerSuit.pin
        || suitHasMoreThan4Tiles tilesPerSuit.sou
        || suitHasMoreThan4Tiles tilesPerSuit.honor


moveWinningTileToEnd : Int -> Array.Array Tile -> Array.Array Tile
moveWinningTileToEnd pos array =
    let
        before =
            Array.slice 0 pos array

        tile =
            Array.get pos array

        after =
            Array.slice (pos + 1) (Array.length array + 1) array
    in
    case tile of
        Just t ->
            Array.append before after
                |> Array.push t

        Nothing ->
            array


removeTileAtPosFromArray : Int -> Array.Array Tile -> Array.Array Tile
removeTileAtPosFromArray pos array =
    let
        before =
            Array.slice 0 pos array

        after =
            Array.slice (pos + 1) (Array.length array + 1) array
    in
    Array.append before after


removeTileAtPosFromList : Int -> List Tile -> List Tile
removeTileAtPosFromList pos tiles =
    Array.fromList tiles
        |> removeTileAtPosFromArray pos
        |> Array.toList


sort : List Tile -> List Tile
sort tiles =
    List.sortBy toComparable tiles


allSuitTiles : Suit -> List Tile
allSuitTiles suit =
    let
        maxN =
            if suit == Honor then
                7

            else
                9
    in
    List.range 1 maxN
        |> List.map (\n -> Tile n suit)


allTiles : List Tile
allTiles =
    allSuitTiles Man ++ allSuitTiles Pin ++ allSuitTiles Sou ++ allSuitTiles Honor


push : Tile -> List Tile -> List Tile
push tile tiles =
    Array.fromList tiles
        |> Array.push tile
        |> Array.toList


toString : Tile -> String
toString tile =
    String.fromInt tile.number ++ Suit.toString tile.suit


randomWind : Random.Generator Wind
randomWind =
    Random.uniform East [ South, West, North ]


handSuit : Parser.Parser (List Tile)
handSuit =
    Parser.map tilesFromSuitString <|
        Parser.getChompedString <|
            Parser.succeed ()
                |. Parser.chompWhile (\c -> Char.isDigit c)
                |. Parser.chompIf (\c -> c == 's' || c == 'm' || c == 'p' || c == 'z')


parseHandHelper : List Tile -> Parser.Parser (Parser.Step (List Tile) (List Tile))
parseHandHelper parsedSuits =
    Parser.oneOf
        [ Parser.succeed (\hand -> Parser.Loop (List.append parsedSuits hand))
            |= handSuit
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done parsedSuits)
        ]


handSuits : Parser.Parser (List Tile)
handSuits =
    Parser.loop [] parseHandHelper


fromString : String -> List Tile
fromString input =
    case Parser.run handSuits input of
        Ok value ->
            value

        Err _ ->
            []


tilesFromSuitString : String -> List Tile
tilesFromSuitString parsedSuit =
    let
        suit =
            String.right 1 parsedSuit |> Suit.fromString

        tiles =
            String.dropRight 1 parsedSuit
                |> String.toList
                |> List.map String.fromChar
                |> List.filterMap String.toInt
    in
    case suit of
        Just s ->
            List.map (\n -> Tile n s) tiles

        Nothing ->
            []


randomList : Int -> Random.Generator ( List Tile, List Tile )
randomList n =
    let
        allPossibleTiles =
            List.concatMap (List.repeat 4) allTiles
    in
    Random.List.choices n allPossibleTiles
        |> Random.andThen
            (\( tiles, remaining ) ->
                Random.pair (Random.constant tiles) (Random.List.shuffle remaining)
            )
