module Page.Waits exposing (Model, Msg, init, update, view)

import Group exposing (Group)
import Html
import Html.Attributes exposing (class, name, type_)
import Html.Events exposing (onClick)
import Random
import Tile exposing (Tile)
import UI


type alias Model =
    { tiles : List Tile
    , waits : List ( Tile, List Group )
    , numberOfNonPairs : Int
    }


type Msg
    = GenerateTiles
    | SetNumberNonPairs Int
    | TilesGenerated (List Tile)


init : ( Model, Cmd Msg )
init =
    ( Model [] [] 1, Random.generate TilesGenerated (Group.randomTenpaiGroups 1) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, Random.generate TilesGenerated (Group.randomTenpaiGroups model.numberOfNonPairs) )

        SetNumberNonPairs num ->
            ( { model | numberOfNonPairs = num }, Cmd.none )

        TilesGenerated tiles ->
            ( { model | tiles = tiles }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ renderNumberTilesSelector model
        , Html.button [ class "button is-primary", onClick GenerateTiles ] [ Html.text "Generate " ]
        , UI.renderTiles False model.tiles
        ]


renderNumberTilesSelector : Model -> Html.Html Msg
renderNumberTilesSelector model =
    let
        createRadioButton text numberOfNonPairs =
            let
                checked =
                    Html.Attributes.checked (model.numberOfNonPairs == numberOfNonPairs)
            in
            Html.label [ class "radio", onClick (SetNumberNonPairs numberOfNonPairs), checked ]
                [ Html.input [ type_ "radio", name "numGroups" ] [], Html.text text ]
    in
    Html.div [ class "control" ]
        [ createRadioButton "4" 1
        , createRadioButton "7" 2
        , createRadioButton "10" 3
        , createRadioButton "13" 4
        ]
