module Page.Waits exposing (Model, Msg, init, update, view)

import Group exposing (Group)
import Html
import Tile exposing (Tile)


type alias Model =
    { tiles : List Tile
    , waits : List ( Tile, List Group )
    }


type Msg
    = GenerateTiles


init : ( Model, Cmd Msg )
init =
    ( Model [] [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div [] []
