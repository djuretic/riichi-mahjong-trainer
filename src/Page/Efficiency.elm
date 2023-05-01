module Page.Efficiency exposing (Model, Msg, init, update, view)

import Html exposing (Html, a, button, div, input, li, p, table, tbody, td, text, tfoot, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import I18n
import Random
import Tile exposing (Tile)
import UI


type alias Model =
    { i18n : I18n.I18n
    , tiles : List Tile
    , availableTiles : List Tile
    , discardedTiles : List Tile
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
            ( { model | tiles = Tile.sort tiles, availableTiles = remainingTiles, discardedTiles = [] }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        numberedTiles =
            False
    in
    div []
        [ div [ class "block" ] [ UI.tiles model.i18n numberedTiles model.tiles ]
        , button [ class "button", onClick GenerateTiles ] [ text (I18n.newHandButton model.i18n) ]
        ]
