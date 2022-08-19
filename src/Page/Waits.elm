module Page.Waits exposing (Model, Msg, init, update, view)

import Group exposing (Group)
import Html
import Random
import Tile exposing (Tile)


type alias Model =
    { tiles : List Tile
    , waits : List ( Tile, List Group )
    }


type Msg
    = GenerateTiles
    | GroupsGenerated (List Group)


init : ( Model, Cmd Msg )
init =
    ( Model [] [], Random.generate GroupsGenerated (Group.randomTenpaiGroups 1) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, Random.generate GroupsGenerated (Group.randomTenpaiGroups 1) )

        GroupsGenerated groups ->
            let
                _ =
                    Debug.log "aaa" groups
            in
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div [] []
