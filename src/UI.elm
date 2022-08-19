module UI exposing (drawTile, renderTiles)

import Html
import Html.Attributes exposing (class, style)
import Tile


renderTiles : Bool -> List Tile.Tile -> Html.Html msg
renderTiles addEmptySpots tiles =
    let
        renderedTiles =
            List.map drawTile tiles

        emptySpots =
            if addEmptySpots then
                List.repeat (14 - List.length renderedTiles) drawBackTile

            else
                []

        allTiles =
            List.append (List.map drawTile tiles) emptySpots
    in
    Html.div [ class "is-flex is-flex-direction-row" ] allTiles


drawTile : Tile.Tile -> Html.Html msg
drawTile tile =
    let
        n =
            String.fromInt tile.number

        isRedDora =
            tile.number == 0

        path =
            if isRedDora then
                case tile.suit of
                    Tile.Sou ->
                        "/img/red-doras/red-dora-bamboo5.png"

                    Tile.Pin ->
                        "/img/red-doras/red-dora-pin5.png"

                    Tile.Man ->
                        "/img/red-doras/red-dora-man5.png"

                    Tile.Honor ->
                        ""

            else
                case tile.suit of
                    Tile.Sou ->
                        "/img/bamboo/bamboo" ++ n ++ ".png"

                    Tile.Pin ->
                        "/img/pin/pin" ++ n ++ ".png"

                    Tile.Man ->
                        "/img/man/man" ++ n ++ ".png"

                    Tile.Honor ->
                        pathHonorTile tile.number
    in
    if String.isEmpty path then
        Html.text ""

    else
        Html.div (tileCss path) []


drawBackTile : Html.Html msg
drawBackTile =
    Html.div (tileCss "/img/face-down-64px.png") []


tileCss : String -> List (Html.Attribute msg)
tileCss path =
    [ style "background-image" ("url(" ++ path ++ ")")
    , style "background-position-x" "-10px"
    , style "height" "64px"
    , style "width" "45px"
    ]


pathHonorTile : Int -> String
pathHonorTile n =
    case n of
        1 ->
            "/img/winds/wind-east.png"

        2 ->
            "/img/winds/wind-south.png"

        3 ->
            "/img/winds/wind-west.png"

        4 ->
            "/img/winds/wind-north.png"

        5 ->
            "/img/dragons/dragon-haku.png"

        6 ->
            "/img/dragons/dragon-green.png"

        7 ->
            "/img/dragons/dragon-chun.png"

        _ ->
            ""
