module Tile exposing
    ( Suit(..)
    , Tile
    , TileNumber
    , Wind(..)
    , deduplicate
    , greenDragonNumber
    , isRun
    , isTriplet
    , partitionBySuit
    , redDragonNumber
    , suitToString
    , whiteDragonNumber
    , windToString
    , windToTileNumber
    )


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
