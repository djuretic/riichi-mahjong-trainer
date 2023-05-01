module Page.Efficiency exposing (Model, Msg, init, update, view)

import Html exposing (Html, a, button, div, input, li, p, table, tbody, td, text, tfoot, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, href, placeholder, target, type_, value)
import Html.Events exposing (onClick)
import I18n
import Random
import Shanten
import Tile exposing (Tile)
import UI


type alias Model =
    { i18n : I18n.I18n
    , tiles : List Tile
    , availableTiles : List Tile
    , discardedTiles : List Tile
    , shanten : Shanten.ShantenDetail
    }


type Msg
    = TilesGenerated ( List Tile, List Tile )
    | GenerateTiles


init : I18n.I18n -> ( Model, Cmd Msg )
init i18n =
    ( { i18n = i18n
      , tiles = []
      , availableTiles = []
      , discardedTiles = []
      , shanten = Shanten.init
      }
    , cmdGenerateTiles
    )


cmdGenerateTiles : Cmd Msg
cmdGenerateTiles =
    Random.generate TilesGenerated (Tile.randomList 14)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, cmdGenerateTiles )

        TilesGenerated ( tiles, remainingTiles ) ->
            let
                shanten =
                    Shanten.shanten tiles
            in
            ( { model | tiles = Tile.sort tiles, availableTiles = remainingTiles, discardedTiles = [], shanten = shanten }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        numberedTiles =
            False

        tilesString =
            Tile.listToString model.tiles
    in
    div []
        [ div [ class "block" ] [ UI.tiles model.i18n numberedTiles model.tiles ]
        , div []
            [ text (String.fromInt model.shanten.final.shanten)
            , text "-shanten -> "
            , a [ href ("https://tenhou.net/2/?q=" ++ tilesString), target "_blank" ] [ text "Tenhou" ]
            , div [] (List.map (\lg -> UI.groupsSimple model.i18n numberedTiles lg) model.shanten.final.groups)
            ]
        , button [ class "button", onClick GenerateTiles ] [ text (I18n.newHandButton model.i18n) ]
        ]
