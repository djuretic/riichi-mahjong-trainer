module Page.Waits exposing (Model, Msg, init, update, view)

import Group exposing (Group)
import Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Tile exposing (Tile)
import UI


type alias Model =
    { tiles : List Tile
    , waits : List ( Tile, List Group )
    }


type Msg
    = GenerateTiles
    | TilesGenerated (List Tile)


init : ( Model, Cmd Msg )
init =
    ( Model [] [], Random.generate TilesGenerated (Group.randomTenpaiGroups 1) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, Random.generate TilesGenerated (Group.randomTenpaiGroups 1) )

        TilesGenerated tiles ->
            ( { model | tiles = tiles }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button [ class "button is-primary", onClick GenerateTiles ] [ Html.text "Generate " ]
        , UI.renderTiles False model.tiles
        ]
