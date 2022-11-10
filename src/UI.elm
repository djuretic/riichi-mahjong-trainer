module UI exposing
    ( breakpoints
    , drawBackTile
    , drawGroups
    , drawGroupsSimple
    , drawTile
    , drawTileSimple
    , icon
    , label
    , renderTiles
    , tileGap
    , tileGapCss
    , tileHeight
    , tileHeightCss
    , tileHeightDoubleCss
    , tilePath
    , tileScale
    , tileTitle
    , tileWidth
    )

import FontAwesome
import Group
import Html
import Html.Attributes exposing (class, src, style, title)
import I18n exposing (I18n)
import List.Extra
import Suit
import Svg.Attributes as SvgA
import Tile


type alias GroupData =
    { group : Group.Group
    , isRepeated : Bool
    , winningTile : Maybe Tile.Tile
    }


breakpoints : Html.Html msg
breakpoints =
    let
        div cls =
            Html.div [ class cls ] [ Html.text cls ]
    in
    Html.div []
        [ div "is-hidden-mobile"
        , div "is-hidden-tablet-only"
        , div "is-hidden-desktop-only"
        , div "is-hidden-widescreen-only"
        , div "is-hidden-fullhd"
        ]


renderTiles : I18n -> Bool -> List Tile.Tile -> Html.Html msg
renderTiles i18n addNumbers tiles =
    let
        allTiles =
            List.map (drawTileSimple i18n addNumbers) tiles
    in
    Html.div [ class "tiles is-flex is-flex-direction-row", tileGapCss ] allTiles


drawTile : I18n -> Bool -> List (Html.Attribute msg) -> Tile.Tile -> Html.Html msg
drawTile i18n addNumbers attrs tile =
    let
        path =
            tilePath addNumbers tile
    in
    if String.isEmpty path then
        Html.text ""

    else
        Html.img (tileCss i18n path (Just tile) |> List.append attrs) []


drawTileSimple : I18n -> Bool -> Tile.Tile -> Html.Html msg
drawTileSimple i18n addNumbers tile =
    drawTile i18n addNumbers [] tile


tilePath : Bool -> Tile.Tile -> String
tilePath addNumbers { number, suit } =
    let
        n =
            String.fromInt number

        n_and_version =
            if addNumbers then
                n ++ "_annotated"

            else
                n
    in
    case suit of
        Suit.Sou ->
            "/img/128px_v2/bamboo/bamboo" ++ n_and_version ++ ".png"

        Suit.Pin ->
            "/img/128px_v2/pin/pin" ++ n_and_version ++ ".png"

        Suit.Man ->
            "/img/128px_v2/man/man" ++ n_and_version ++ ".png"

        Suit.Honor ->
            pathHonorTile addNumbers number


drawBackTile : I18n -> Html.Html msg
drawBackTile i18n =
    Html.img (tileCss i18n "/img/128px_v2/face-down-128px.png" Nothing) []


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


tileCss : I18n -> String -> Maybe Tile.Tile -> List (Html.Attribute msg)
tileCss i18n path tile =
    [ src path
    , class "tile"
    , title (Maybe.map (tileTitle i18n) tile |> Maybe.withDefault "")

    -- needed for nested flex to work when shrinking
    , style "min-width" "20px"
    ]


pathHonorTile : Bool -> Int -> String
pathHonorTile addNumbers n =
    let
        extra =
            if addNumbers then
                "_annotated"

            else
                ""
    in
    case n of
        1 ->
            "/img/128px_v2/winds/wind-east" ++ extra ++ ".png"

        2 ->
            "/img/128px_v2/winds/wind-south" ++ extra ++ ".png"

        3 ->
            "/img/128px_v2/winds/wind-west" ++ extra ++ ".png"

        4 ->
            "/img/128px_v2/winds/wind-north" ++ extra ++ ".png"

        5 ->
            "/img/128px_v2/dragons/dragon-haku" ++ extra ++ ".png"

        6 ->
            "/img/128px_v2/dragons/dragon-green" ++ extra ++ ".png"

        7 ->
            "/img/128px_v2/dragons/dragon-chun" ++ extra ++ ".png"

        _ ->
            ""


drawGroups : I18n -> Bool -> Tile.Tile -> List Group.Group -> Html.Html msg
drawGroups i18n addNumbers winTile groups =
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
        (List.map (\{ group, winningTile } -> drawGroup i18n addNumbers [] winningTile group) groupsWithRepeatedInfo)


drawGroup : I18n -> Bool -> List (Html.Attribute msg) -> Maybe Tile.Tile -> Group.Group -> Html.Html msg
drawGroup i18n addNumbers attrs winningTile group =
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
        (List.map (\( t, atts ) -> drawTile i18n addNumbers atts t) tilesWithWinInfo)


drawGroupsSimple : I18n -> Bool -> List Group.Group -> Html.Html msg
drawGroupsSimple i18n addNumbers groups =
    Html.div [ class "groups is-flex is-flex-direction-row", groupGapCss, tileHeightCss ] (List.map (drawGroup i18n addNumbers [] Nothing) groups)


winningTileCss : Html.Attribute msg
winningTileCss =
    Html.Attributes.style "filter" "sepia(50%)"


icon : String -> FontAwesome.Icon hasId -> Html.Html msg
icon classes icn =
    FontAwesome.styled [ SvgA.class classes ] icn |> FontAwesome.view


tileTitle : I18n -> Tile.Tile -> String
tileTitle i18n tile =
    if tile.suit == Suit.Honor then
        case tile.number of
            1 ->
                I18n.eastWindDescription i18n

            2 ->
                I18n.southWindDescription i18n

            3 ->
                I18n.westWindDescription i18n

            4 ->
                I18n.northWindDescription i18n

            5 ->
                I18n.whiteDragonDescription i18n

            6 ->
                I18n.greenDragonDescription i18n

            7 ->
                I18n.redDragonDescription i18n

            _ ->
                "?"

    else
        let
            n =
                case tile.number of
                    1 ->
                        I18n.tileNumber1 i18n

                    2 ->
                        I18n.tileNumber2 i18n

                    3 ->
                        I18n.tileNumber3 i18n

                    4 ->
                        I18n.tileNumber4 i18n

                    5 ->
                        I18n.tileNumber5 i18n

                    6 ->
                        I18n.tileNumber6 i18n

                    7 ->
                        I18n.tileNumber7 i18n

                    8 ->
                        I18n.tileNumber8 i18n

                    9 ->
                        I18n.tileNumber9 i18n

                    _ ->
                        "?"
        in
        case tile.suit of
            Suit.Man ->
                I18n.manTileDescription n i18n

            Suit.Pin ->
                I18n.pinTileDescription n i18n

            Suit.Sou ->
                I18n.souTileDescription n i18n

            _ ->
                "?"


label : String -> Html.Html msg -> Html.Html msg
label labelText content =
    Html.div [ class "field is-horizontal" ]
        [ Html.div [ class "field-label" ] [ Html.label [ class "label" ] [ Html.text labelText ] ]
        , Html.div [ class "field-body" ] [ content ]
        ]
