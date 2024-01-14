module UI exposing
    ( TileMode(..)
    , UIMsg(..)
    , backTile
    , breakpoints
    , groups
    , groupsSimple
    , icon
    , label
    , tileGap
    , tileGapAttr
    , tileHeight
    , tileHeightCss
    , tileHeightDoubleCss
    , tileMinWidth
    , tilePath
    , tileScale
    , tileSimple
    , tileSimpleMinWidth
    , tileTitle
    , tileWidth
    , tilesDiv
    , tilesDivAttrs
    , tilesDivMinWidth
    , tilesDivWithOnClick
    , tilesDivWithOnClickAndAttrs
    , tilesList
    , tilesListMinWidth
    , tilesListWithOnClick
    )

import FontAwesome
import Group
import Html
import Html.Attributes exposing (class, src, style, title)
import Html.Events exposing (onClick)
import I18n exposing (I18n)
import List.Extra
import Suit
import Svg.Attributes as SvgA
import Tile


type alias GroupData =
    { group : Group.Group
    , winningTile : Maybe Tile.Tile
    }


type UIMsg
    = TileOnClick Tile.Tile


type TileMode
    = TileMinWidth
    | TileFixedWidth Int


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


tilesDivMinWidth : I18n -> Bool -> List Tile.Tile -> Html.Html msg
tilesDivMinWidth i18n addNumbers baseTiles =
    tilesDiv i18n addNumbers TileMinWidth baseTiles


tilesDiv : I18n -> Bool -> TileMode -> List Tile.Tile -> Html.Html msg
tilesDiv i18n addNumbers tileMode baseTiles =
    let
        allTiles =
            tilesList i18n addNumbers tileMode baseTiles
    in
    Html.div tilesDivAttrs allTiles


tilesDivAttrs : List (Html.Attribute msg)
tilesDivAttrs =
    [ class "tiles is-flex is-flex-direction-row", tileGapAttr ]


tilesListMinWidth : I18n -> Bool -> List Tile.Tile -> List (Html.Html msg)
tilesListMinWidth i18n addNumbers baseTiles =
    List.map (tileSimpleMinWidth i18n addNumbers) baseTiles


tilesList : I18n -> Bool -> TileMode -> List Tile.Tile -> List (Html.Html msg)
tilesList i18n addNumbers tileMode baseTiles =
    List.map (tile i18n addNumbers tileMode []) baseTiles


tilesDivWithOnClick : I18n -> Bool -> List Tile.Tile -> Html.Html UIMsg
tilesDivWithOnClick i18n addNumbers baseTiles =
    let
        allTiles =
            tilesListWithOnClick i18n addNumbers baseTiles
    in
    Html.div tilesDivAttrs allTiles


tilesDivWithOnClickAndAttrs : I18n -> Bool -> (Tile.Tile -> List (Html.Attribute UIMsg)) -> List Tile.Tile -> Html.Html UIMsg
tilesDivWithOnClickAndAttrs i18n addNumbers attrsFn baseTiles =
    let
        allTiles =
            tilesListWithOnClickAndAttrs i18n addNumbers attrsFn baseTiles
    in
    Html.div tilesDivAttrs allTiles


tilesListWithOnClick : I18n -> Bool -> List Tile.Tile -> List (Html.Html UIMsg)
tilesListWithOnClick i18n addNumbers baseTiles =
    List.map (tileWithOnClick i18n addNumbers []) baseTiles


tilesListWithOnClickAndAttrs : I18n -> Bool -> (Tile.Tile -> List (Html.Attribute UIMsg)) -> List Tile.Tile -> List (Html.Html UIMsg)
tilesListWithOnClickAndAttrs i18n addNumbers attrsFn baseTiles =
    List.map (\t -> tileWithOnClick i18n addNumbers (attrsFn t) t) baseTiles


tileMinWidth : I18n -> Bool -> List (Html.Attribute msg) -> Tile.Tile -> Html.Html msg
tileMinWidth i18n addNumbers attrs baseTile =
    tile i18n addNumbers TileMinWidth attrs baseTile


tile : I18n -> Bool -> TileMode -> List (Html.Attribute msg) -> Tile.Tile -> Html.Html msg
tile i18n addNumbers tileMode attrs baseTile =
    let
        path =
            tilePath addNumbers baseTile
    in
    if String.isEmpty path then
        Html.text ""

    else
        Html.img (tileAttrs i18n path tileMode (Just baseTile) |> List.append attrs) []


tileWithOnClick : I18n -> Bool -> List (Html.Attribute UIMsg) -> Tile.Tile -> Html.Html UIMsg
tileWithOnClick i18n addNumbers attrs baseTile =
    let
        path =
            tilePath addNumbers baseTile
    in
    if String.isEmpty path then
        Html.text ""

    else
        Html.img (tileAttrs i18n path TileMinWidth (Just baseTile) ++ attrs ++ [ class "is-clickable", onClick (TileOnClick baseTile) ]) []


tileSimpleMinWidth : I18n -> Bool -> Tile.Tile -> Html.Html msg
tileSimpleMinWidth i18n addNumbers baseTile =
    tileMinWidth i18n addNumbers [] baseTile


tileSimple : I18n -> Bool -> TileMode -> Tile.Tile -> Html.Html msg
tileSimple i18n addNumbers tileMode baseTile =
    tile i18n addNumbers tileMode [] baseTile


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


backTile : I18n -> List (Html.Attribute msg) -> Html.Html msg
backTile i18n attrs =
    Html.img (List.append attrs (tileAttrs i18n "/img/128px_v2/face-down-128px.png" TileMinWidth Nothing)) []


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


tileGapAttr : Html.Attribute msg
tileGapAttr =
    Html.Attributes.style "gap" (String.fromInt tileGap ++ "px")


groupGap : Int
groupGap =
    10


groupGapAttr : Html.Attribute msg
groupGapAttr =
    Html.Attributes.style "gap" (String.fromInt groupGap ++ "px")


tileAttrs : I18n -> String -> TileMode -> Maybe Tile.Tile -> List (Html.Attribute msg)
tileAttrs i18n path tileMode baseTile =
    let
        styleWidth =
            case tileMode of
                TileMinWidth ->
                    -- needed for nested flex to work when shrinking
                    style "min-width" "20px"

                TileFixedWidth w ->
                    style "width" (String.fromInt w ++ "px")
    in
    [ src path
    , class "tile"
    , title (Maybe.map (tileTitle i18n) baseTile |> Maybe.withDefault "")
    , styleWidth
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


groups : I18n -> Bool -> Tile.Tile -> List Group.Group -> Html.Html msg
groups i18n addNumbers winTile baseGroups =
    let
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
            List.map (\g -> { group = g, winningTile = Nothing }) baseGroups
                |> addCointainsWinningTile
    in
    Html.div [ class "groups is-flex is-flex-direction-row", groupGapAttr ]
        (List.map (\groupInfo -> group i18n addNumbers [] groupInfo.winningTile groupInfo.group) groupsWithRepeatedInfo)


group : I18n -> Bool -> List (Html.Attribute msg) -> Maybe Tile.Tile -> Group.Group -> Html.Html msg
group i18n addNumbers attrs winningTile baseGroup =
    let
        baseTiles : List ( Tile.Tile, List (Html.Attribute msg) )
        baseTiles =
            Group.toTiles baseGroup
                |> List.map (\t -> ( t, [] ))

        tilesWithWinInfo =
            case winningTile of
                Just winTile ->
                    let
                        pos =
                            List.Extra.elemIndices ( winTile, [] ) baseTiles
                                |> List.Extra.last
                    in
                    case pos of
                        Just i ->
                            List.Extra.updateAt i (\( t, _ ) -> ( t, [ winningTileCss ] )) baseTiles

                        Nothing ->
                            baseTiles

                Nothing ->
                    baseTiles
    in
    Html.div (List.append [ class "group is-flex is-flex-direction-row", tileGapAttr ] attrs)
        (List.map (\( t, atts ) -> tileMinWidth i18n addNumbers atts t) tilesWithWinInfo)


groupsSimple : I18n -> Bool -> List Group.Group -> Html.Html msg
groupsSimple i18n addNumbers baseGroups =
    Html.div [ class "groups is-flex is-flex-direction-row", groupGapAttr, tileHeightCss ] (List.map (group i18n addNumbers [] Nothing) baseGroups)


winningTileCss : Html.Attribute msg
winningTileCss =
    Html.Attributes.style "filter" "sepia(50%)"


icon : String -> FontAwesome.Icon hasId -> Html.Html msg
icon classes icn =
    FontAwesome.styled [ SvgA.class classes ] icn |> FontAwesome.view


tileTitle : I18n -> Tile.Tile -> String
tileTitle i18n baseTile =
    if baseTile.suit == Suit.Honor then
        case baseTile.number of
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
                case baseTile.number of
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
        case baseTile.suit of
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
