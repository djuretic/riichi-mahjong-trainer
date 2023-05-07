module Page.Efficiency exposing (Model, Msg, init, update, view)

import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, classList, href, target)
import Html.Events exposing (onClick)
import I18n
import List.Extra
import Random
import Random.List
import Shanten
import Tile exposing (Tile)
import UI


type alias Model =
    { i18n : I18n.I18n
    , tiles : List Tile
    , availableTiles : List Tile
    , discardedTiles : List Tile
    , shanten : Shanten.ShantenDetail
    , showShanten : Bool
    , tileAcceptance : Shanten.TileAcceptance
    }


type Msg
    = GenerateTiles
    | TilesGenerated ( List Tile, List Tile )
    | DiscardTile Tile
    | DrawTile ( Maybe Tile, List Tile )
    | ToggleShowShanten


init : I18n.I18n -> ( Model, Cmd Msg )
init i18n =
    ( { i18n = i18n
      , tiles = []
      , availableTiles = []
      , discardedTiles = []
      , shanten = Shanten.init
      , showShanten = False
      , tileAcceptance = Shanten.Draw Shanten.emptyTileAcceptanceDetail
      }
    , cmdGenerateTiles
    )


cmdGenerateTiles : Cmd Msg
cmdGenerateTiles =
    Random.generate TilesGenerated (Tile.randomList 14)


recalculateShanten : Model -> Model
recalculateShanten model =
    { model | shanten = Shanten.shanten model.tiles, tileAcceptance = Shanten.tileAcceptance model.tiles }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, cmdGenerateTiles )

        TilesGenerated ( tiles, remainingTiles ) ->
            ( recalculateShanten { model | tiles = Tile.sort tiles, availableTiles = remainingTiles, discardedTiles = [] }, Cmd.none )

        DiscardTile tile ->
            if List.member tile model.tiles then
                ( { model
                    | tiles = List.Extra.remove tile model.tiles |> Tile.sort
                    , discardedTiles = model.discardedTiles ++ [ tile ]
                    , showShanten = False
                  }
                , Random.generate DrawTile (Random.List.choose model.availableTiles)
                )

            else
                ( model, Cmd.none )

        DrawTile ( possibleTile, availableTiles ) ->
            case possibleTile of
                Just tile ->
                    ( recalculateShanten { model | tiles = model.tiles ++ [ tile ], availableTiles = availableTiles }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleShowShanten ->
            ( { model | showShanten = not model.showShanten }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        numberedTiles =
            False

        tilesString =
            Tile.listToString model.tiles

        uiMap uiMsg =
            case uiMsg of
                UI.TileOnClick tile ->
                    DiscardTile tile
    in
    div []
        [ div [ class "block" ] [ UI.tilesWithOnClick model.i18n numberedTiles model.tiles |> Html.map uiMap ]
        , button [ class "button", onClick GenerateTiles ] [ text (I18n.newHandButton model.i18n) ]
        , button [ class "button", onClick ToggleShowShanten ] [ text "Shanten" ]
        , div [ classList [ ( "is-invisible", not model.showShanten ) ] ]
            [ text (String.fromInt model.shanten.final.shanten)
            , text "-shanten -> "
            , a [ href ("https://tenhou.net/2/?q=" ++ tilesString), target "_blank" ] [ text "Tenhou" ]
            , div [] (List.map (\lg -> UI.groupsSimple model.i18n numberedTiles lg) model.shanten.final.groups)
            , text "Tile acceptance"
            , tileAcceptance model
            ]
        ]


tileAcceptance : Model -> Html Msg
tileAcceptance model =
    let
        addNumbers =
            False
    in
    case model.tileAcceptance of
        Shanten.Draw _ ->
            div [] []

        Shanten.DiscardAndDraw listAcceptance ->
            div []
                (List.map
                    (\( t, detail ) ->
                        div []
                            [ UI.tileSimple model.i18n addNumbers t
                            , text "->"
                            , UI.tiles model.i18n addNumbers detail.tiles
                            , text (String.fromInt detail.numTiles)
                            ]
                    )
                    listAcceptance
                )
