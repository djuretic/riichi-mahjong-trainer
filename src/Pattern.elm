module Pattern exposing (Pattern, match, referenceUrl)

import List.Extra
import Suit
import Tile


type Side
    = Left
    | Right


type SanmentanStart
    = One
    | Two
    | Three


type Pattern
    = Nobetan Tile.Tile
    | Pentan Side Suit.Suit
    | Kantan Side Tile.Tile
    | Ryantan Side Tile.Tile
    | Aryanmen Side Tile.Tile
    | Shanpon Tile.Tile Tile.Tile
    | Sanmentan SanmentanStart Suit.Suit
    | Kantankan Tile.Tile
    | Tatsumaki Tile.Tile


toTiles : Pattern -> List Tile.Tile
toTiles pattern =
    case pattern of
        Nobetan tile ->
            List.range tile.number (tile.number + 3)
                |> List.map (\n -> Tile.Tile n tile.suit)

        Pentan side suit ->
            case side of
                Left ->
                    List.map (\n -> Tile.Tile n suit) [ 1, 2, 2, 2 ]

                Right ->
                    List.map (\n -> Tile.Tile n suit) [ 8, 8, 8, 9 ]

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

        Sanmentan start suit ->
            let
                startNumber =
                    case start of
                        One ->
                            1

                        Two ->
                            2

                        Three ->
                            3
            in
            List.map (\n -> Tile.Tile (n + startNumber) suit) [ 0, 1, 2, 3, 4, 5, 6 ]

        Kantankan tile ->
            List.map (\n -> Tile.Tile (n + tile.number) tile.suit) [ 0, 0, 0, 2, 4, 4, 4 ]

        Tatsumaki tile ->
            List.map (\n -> Tile.Tile (n + tile.number) tile.suit) [ 0, 0, 0, 1, 2, 2, 2 ]


{-| Does not include patterns with 2 tiles/suits (e.g. shanpon)
-}
fourTilePatterns : Tile.TileNumber -> Suit.Suit -> List Pattern
fourTilePatterns lowestTileNumber suit =
    let
        tile =
            Tile.Tile lowestTileNumber suit
    in
    [ Nobetan tile
    , Pentan Left tile.suit
    , Pentan Right tile.suit
    , Kantan Left tile
    , Kantan Right tile
    , Ryantan Left tile
    , Ryantan Right tile
    , Aryanmen Left tile
    , Aryanmen Right tile
    ]


sevenTilePatterns : Tile.TileNumber -> Suit.Suit -> List Pattern
sevenTilePatterns lowestTileNumber suit =
    let
        tile =
            Tile.Tile lowestTileNumber suit
    in
    [ Sanmentan One suit
    , Sanmentan Two suit
    , Sanmentan Three suit
    , Kantankan tile
    , Tatsumaki tile
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


matchFourTiles : List Tile.Tile -> Maybe Pattern
matchFourTiles tiles =
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
                    fourTilePatterns tile.number tile.suit
                        |> List.Extra.find (\pattern -> toTiles pattern == tiles)
                )
                firstTile


matchSevenTiles : List Tile.Tile -> Maybe Pattern
matchSevenTiles tiles =
    let
        firstTile =
            List.head tiles
    in
    Maybe.andThen
        (\tile ->
            sevenTilePatterns tile.number tile.suit
                |> List.Extra.find (\pattern -> toTiles pattern == tiles)
        )
        firstTile


{-| Only works on sorted input
-}
match : List Tile.Tile -> Maybe Pattern
match tiles =
    case List.length tiles of
        4 ->
            matchFourTiles tiles

        7 ->
            matchSevenTiles tiles

        _ ->
            Nothing


referenceUrl : Pattern -> Maybe ( String, String )
referenceUrl pattern =
    let
        riichiWiki =
            "https://riichi.wiki/"
    in
    case pattern of
        Nobetan _ ->
            Just ( "Nobetan", riichiWiki ++ "Nobetan" )

        Pentan _ _ ->
            Just ( "Pentan", riichiWiki ++ "Pentan" )

        Kantan _ _ ->
            Just ( "Kantan", riichiWiki ++ "Kantan" )

        Ryantan _ _ ->
            Just ( "Ryantan", riichiWiki ++ "Ryantan" )

        Aryanmen _ _ ->
            Just ( "Aryanmen", riichiWiki ++ "Aryanmen" )

        Shanpon _ _ ->
            Just ( "Shanpon", riichiWiki ++ "Shanpon" )

        Sanmentan _ _ ->
            Just ( "Sanmentan", riichiWiki ++ "Sanmentan" )

        Kantankan _ ->
            -- Kantankan redirects to Ryantan in riichi wiki
            Nothing

        Tatsumaki _ ->
            Just ( "Tatsumaki", riichiWiki ++ "Tatsumaki" )
