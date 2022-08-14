module Tile exposing
    ( Suit(..)
    , Tile
    , TileNumber
    , Wind(..)
    , deduplicate
    , greenDragonNumber
    , hasMoreThan4Tiles
    , isRun
    , isTriplet
    , moveWinningTileToEnd
    , partitionBySuit
    , redDragonNumber
    , removeTileAtPos
    , sort
    , suitToString
    , toArrayCounter
    , toComparable
    , whiteDragonNumber
    , windToString
    , windToTileNumber
    )

import Array
import Counter


type Suit
    = Sou
    | Man
    | Pin
    | Honor


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
    ( suitToString tile.suit, tile.number )


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


suitToString : Suit -> String
suitToString suit =
    case suit of
        Man ->
            "m"

        Pin ->
            "p"

        Sou ->
            "s"

        Honor ->
            "z"


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


toArrayCounter : List TileNumber -> Counter.Counter
toArrayCounter tileNumbers =
    let
        counter =
            Array.initialize 9 (always 0)

        accum : TileNumber -> Array.Array Int -> Array.Array Int
        accum n cnt =
            Array.set (n - 1) (Maybe.withDefault 0 (Array.get (n - 1) cnt) + 1) cnt
    in
    List.foldl accum counter tileNumbers


hasMoreThan4Tiles : List Tile -> Bool
hasMoreThan4Tiles tiles =
    let
        tilesPerSuit =
            partitionBySuit tiles

        suitHasMoreThan4Tiles : List Tile -> Bool
        suitHasMoreThan4Tiles suitTiles =
            List.map .number suitTiles
                |> toArrayCounter
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


removeTileAtPos : Int -> Array.Array Tile -> Array.Array Tile
removeTileAtPos pos array =
    let
        before =
            Array.slice 0 pos array

        after =
            Array.slice (pos + 1) (Array.length array + 1) array
    in
    Array.append before after


sort : List Tile -> List Tile
sort tiles =
    List.sortBy toComparable tiles
