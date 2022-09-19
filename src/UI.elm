module UI exposing
    ( drawGroups
    , drawGroupsSimple
    , drawTile
    , drawTileSimple
    , renderTiles
    , tileGap
    , tileGapCss
    , tileHeight
    , tileHeightCss
    , tileHeightDoubleCss
    , tilePath
    , tileScale
    , tileWidth
    )

import Group
import Html
import Html.Attributes exposing (class, src, style)
import List.Extra
import Tile


type alias GroupData =
    { group : Group.Group
    , isRepeated : Bool
    , winningTile : Maybe Tile.Tile
    }


renderTiles : Bool -> List Tile.Tile -> Html.Html msg
renderTiles addEmptySpots tiles =
    let
        renderedTiles =
            List.map drawTileSimple tiles

        emptySpots =
            if addEmptySpots then
                List.repeat (14 - List.length renderedTiles) drawBackTile

            else
                []

        allTiles =
            List.append (List.map drawTileSimple tiles) emptySpots
    in
    Html.div [ class "tiles is-flex is-flex-direction-row", tileGapCss ] allTiles


drawTile : List (Html.Attribute msg) -> Tile.Tile -> Html.Html msg
drawTile attrs tile =
    let
        path =
            tilePath tile
    in
    if String.isEmpty path then
        Html.text ""

    else
        Html.img (tileCss path |> List.append attrs) []


drawTileSimple : Tile.Tile -> Html.Html msg
drawTileSimple tile =
    drawTile [] tile


tilePath : Tile.Tile -> String
tilePath { number, suit } =
    let
        n =
            String.fromInt number

        isRedDora =
            number == 0
    in
    if isRedDora then
        case suit of
            Tile.Sou ->
                "/img/128px_v2/red-doras/red-dora-bamboo5.png"

            Tile.Pin ->
                "/img/128px_v2/red-doras/red-dora-pin5.png"

            Tile.Man ->
                "/img/128px_v2/red-doras/red-dora-man5.png"

            Tile.Honor ->
                ""

    else
        case suit of
            Tile.Sou ->
                "/img/128px_v2/bamboo/bamboo" ++ n ++ ".png"

            Tile.Pin ->
                "/img/128px_v2/pin/pin" ++ n ++ ".png"

            Tile.Man ->
                "/img/128px_v2/man/man" ++ n ++ ".png"

            Tile.Honor ->
                pathHonorTile number


drawBackTile : Html.Html msg
drawBackTile =
    Html.div (tileCss "/img/face-down-64px.png") []


tileScale : Float
tileScale =
    1.5


tileWidth : Int
tileWidth =
    toFloat 41 * tileScale |> round


tileHeight : Int
tileHeight =
    toFloat 64 * tileScale |> round


tileHeightCss : Html.Attribute msg
tileHeightCss =
    style "height" (String.fromInt tileHeight ++ "px")


tileHeightDoubleCss : Html.Attribute msg
tileHeightDoubleCss =
    style "height" (String.fromInt (2 * tileHeight) ++ "px")


{-| Gap between individual tiles
-}
tileGap : Int
tileGap =
    2


tileGapCss : Html.Attribute msg
tileGapCss =
    Html.Attributes.style "gap" (String.fromInt tileGap ++ "px")


groupGap : Int
groupGap =
    10


groupGapCss : Html.Attribute msg
groupGapCss =
    Html.Attributes.style "gap" (String.fromInt groupGap ++ "px")


tileCss : String -> List (Html.Attribute msg)
tileCss path =
    [ src path

    --     style "background-image" ("url(" ++ path ++ "), url(/img/128px/placeholder.png)")
    -- , style "background-position-x" "49%, 0px"
    -- , style "background-repeat" "no-repeat, no-repeat"
    -- , style "background-size" "152%, 100%"
    -- , style "height" (String.fromInt tileHeight ++ "px")
    , style "width" (String.fromInt tileWidth ++ "px")

    -- needed for nested flex to work when shrinking
    , style "min-width" "20px"
    ]


pathHonorTile : Int -> String
pathHonorTile n =
    case n of
        1 ->
            "/img/128px_v2/winds/wind-east.png"

        2 ->
            "/img/128px_v2/winds/wind-south.png"

        3 ->
            "/img/128px_v2/winds/wind-west.png"

        4 ->
            "/img/128px_v2/winds/wind-north.png"

        5 ->
            "/img/128px_v2/dragons/dragon-haku.png"

        6 ->
            "/img/128px_v2/dragons/dragon-green.png"

        7 ->
            "/img/128px_v2/dragons/dragon-chun.png"

        _ ->
            ""


drawGroups : Tile.Tile -> List Group.Group -> Html.Html msg
drawGroups winTile groups =
    let
        -- unused
        addGroupIsRepeatedData sg lg =
            case lg of
                [] ->
                    []

                x :: xs ->
                    if List.Extra.find (\e -> e == x) sg /= Nothing then
                        { group = x, isRepeated = True, winningTile = Nothing } :: addGroupIsRepeatedData (List.Extra.remove x sg) xs

                    else
                        { group = x, isRepeated = False, winningTile = Nothing } :: addGroupIsRepeatedData sg xs

        addCointainsWinningTile : List GroupData -> List GroupData
        addCointainsWinningTile groupsData =
            let
                pos =
                    List.Extra.findIndices (\g -> Group.member winTile g.group) groupsData
                        |> List.Extra.last
            in
            case pos of
                Just i ->
                    List.Extra.updateAt i (\g -> { g | winningTile = Just winTile }) groupsData

                Nothing ->
                    groupsData

        groupsWithRepeatedInfo =
            addGroupIsRepeatedData [] groups
                |> addCointainsWinningTile
    in
    Html.div [ class "groups is-flex is-flex-direction-row", groupGapCss ]
        (List.map (\{ group, winningTile } -> drawGroup [] winningTile group) groupsWithRepeatedInfo)


drawGroup : List (Html.Attribute msg) -> Maybe Tile.Tile -> Group.Group -> Html.Html msg
drawGroup attrs winningTile group =
    let
        tiles : List ( Tile.Tile, List (Html.Attribute msg) )
        tiles =
            Group.toTiles group
                |> List.map (\t -> ( t, [] ))

        tilesWithWinInfo =
            case winningTile of
                Just winTile ->
                    let
                        pos =
                            List.Extra.elemIndices ( winTile, [] ) tiles
                                |> List.Extra.last
                    in
                    case pos of
                        Just i ->
                            List.Extra.updateAt i (\( t, _ ) -> ( t, [ winningTileCss ] )) tiles

                        Nothing ->
                            tiles

                Nothing ->
                    tiles
    in
    Html.div (List.append [ class "group is-flex is-flex-direction-row", tileGapCss ] attrs)
        (List.map (\( t, atts ) -> drawTile atts t) tilesWithWinInfo)


drawGroupsSimple : List Group.Group -> Html.Html msg
drawGroupsSimple groups =
    Html.div [ class "groups is-flex is-flex-direction-row", groupGapCss, tileHeightCss ] (List.map (drawGroup [] Nothing) groups)


winningTileCss : Html.Attribute msg
winningTileCss =
    Html.Attributes.style "filter" "sepia(50%)"
