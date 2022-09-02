module UI exposing (drawGroups, drawGroupsSimple, drawTile, drawTileSimple, renderTiles, tilePath)

import Group
import Html
import Html.Attributes exposing (class, style)
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
    Html.div [ class "is-flex is-flex-direction-row" ] allTiles


drawTile : List (Html.Attribute msg) -> Tile.Tile -> Html.Html msg
drawTile attrs tile =
    let
        path =
            tilePath tile
    in
    if String.isEmpty path then
        Html.text ""

    else
        Html.div (tileCss path |> List.append attrs) []


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
                "/img/red-doras/red-dora-bamboo5.png"

            Tile.Pin ->
                "/img/red-doras/red-dora-pin5.png"

            Tile.Man ->
                "/img/red-doras/red-dora-man5.png"

            Tile.Honor ->
                ""

    else
        case suit of
            Tile.Sou ->
                "/img/bamboo/bamboo" ++ n ++ ".png"

            Tile.Pin ->
                "/img/pin/pin" ++ n ++ ".png"

            Tile.Man ->
                "/img/man/man" ++ n ++ ".png"

            Tile.Honor ->
                pathHonorTile number


drawBackTile : Html.Html msg
drawBackTile =
    Html.div (tileCss "/img/face-down-64px.png") []


tileCss : String -> List (Html.Attribute msg)
tileCss path =
    [ style "background-image" ("url(" ++ path ++ ")")
    , style "background-position-x" "-11px"
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


drawGroups : List Group.Group -> Tile.Tile -> List Group.Group -> Html.Html msg
drawGroups specialGroups winTile groups =
    let
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
            addGroupIsRepeatedData specialGroups groups
                |> addCointainsWinningTile

        css isRepeated =
            if isRepeated then
                style "opacity" "0.5"

            else
                class ""
    in
    Html.div [ class "is-flex is-flex-direction-row is-flex-wrap-wrap" ]
        (List.map (\{ group, isRepeated, winningTile } -> drawGroup [ css isRepeated ] winningTile group) groupsWithRepeatedInfo)


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
    Html.div (List.append [ class "is-flex is-flex-direction-row", style "padding-right" "10px" ] attrs)
        (List.map (\( t, atts ) -> drawTile atts t) tilesWithWinInfo)


drawGroupsSimple : List Group.Group -> Html.Html msg
drawGroupsSimple groups =
    Html.div [ class "is-flex is-flex-direction-row is-flex-wrap-wrap" ] (List.map (drawGroup [] Nothing) groups)


winningTileCss =
    Html.Attributes.style "filter" "sepia(50%)"
