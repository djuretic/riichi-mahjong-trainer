module Pattern exposing (match)

import List.Extra
import Suit
import Tile


type Side
    = Left
    | Right


type Pattern
    = Nobetan Tile.Tile
    | Pentan Side Tile.Tile
    | Kantan Side Tile.Tile
    | Ryantan Side Tile.Tile
    | Aryanmen Side Tile.Tile
    | Shanpon Tile.Tile Tile.Tile


toTiles : Pattern -> List Tile.Tile
toTiles pattern =
    case pattern of
        Nobetan tile ->
            List.range tile.number (tile.number + 3)
                |> List.map (\n -> Tile.Tile n tile.suit)

        Pentan side tile ->
            case side of
                Left ->
                    List.map (\n -> Tile.Tile n tile.suit) [ 1, 2, 2, 2 ]

                Right ->
                    List.map (\n -> Tile.Tile n tile.suit) [ 8, 8, 8, 9 ]

        Kantan side tile ->
            case side of
                Left ->
                    List.map (\n -> Tile.Tile (n + tile.number) tile.suit) [ 0, 2, 2, 2 ]

                Right ->
                    List.map (\n -> Tile.Tile (n + tile.number) tile.suit) [ 0, 0, 0, 2 ]

        Ryantan side tile ->
            case side of
                Left ->
                    List.map (\n -> Tile.Tile (n + tile.number) tile.suit) [ 0, 1, 1, 1 ]

                Right ->
                    List.map (\n -> Tile.Tile (n + tile.number) tile.suit) [ 0, 0, 0, 1 ]

        Aryanmen side tile ->
            case side of
                Left ->
                    List.map (\n -> Tile.Tile (n + tile.number) tile.suit) [ 0, 0, 1, 2 ]

                Right ->
                    List.map (\n -> Tile.Tile (n + tile.number) tile.suit) [ 0, 1, 2, 2 ]

        Shanpon tile1 tile2 ->
            [ tile1, tile1, tile2, tile2 ]


{-| Does not include patterns with 2 tiles/suits (e.g. shanpon)
-}
all : Tile.TileNumber -> Suit.Suit -> List Pattern
all lowestTileNumber suit =
    let
        tile =
            Tile.Tile lowestTileNumber suit
    in
    [ Nobetan tile
    , Pentan Left tile
    , Pentan Right tile
    , Kantan Left tile
    , Kantan Right tile
    , Ryantan Left tile
    , Ryantan Right tile
    , Aryanmen Left tile
    , Aryanmen Right tile
    ]


tilesToShanpon : List Tile.Tile -> Maybe Pattern
tilesToShanpon tiles =
    case tiles of
        [ tile1, tile2, tile3, tile4 ] ->
            if tile1 == tile2 && tile3 == tile4 && tile1 /= tile3 then
                Just (Shanpon tile1 tile3)

            else
                Nothing

        _ ->
            Nothing


{-| Only works on sorted input
-}
match : List Tile.Tile -> Maybe Pattern
match tiles =
    let
        firstTile =
            List.head tiles
    in
    case tilesToShanpon tiles of
        Just pattern ->
            Just pattern

        Nothing ->
            Maybe.andThen
                (\tile ->
                    all tile.number tile.suit
                        |> List.Extra.find (\pattern -> toTiles pattern == tiles)
                )
                firstTile
