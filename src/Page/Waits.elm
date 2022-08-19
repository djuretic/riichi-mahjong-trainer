module Page.Waits exposing (Model, Msg, init, update, view)

import Group exposing (Group)
import Html exposing (Html, button, div, p, table, tbody, td, tr)
import Html.Attributes exposing (class, name, style, type_)
import Html.Events exposing (onClick)
import List.Extra
import Random
import Set exposing (Set)
import Tile exposing (Tile)
import UI


type alias Model =
    { tiles : List Tile
    , waits : List ( Tile, List Group )
    , numberOfNonPairs : Int
    , selectedWaits : Set Tile.ComparableTile
    }


type Msg
    = GenerateTiles
    | SetNumberNonPairs Int
    | TilesGenerated (List Tile)
    | ToggleWaitTile Tile


init : ( Model, Cmd Msg )
init =
    ( { tiles = [], waits = [], numberOfNonPairs = 1, selectedWaits = Set.empty }
    , Random.generate TilesGenerated (Group.randomTenpaiGroups 1)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, Random.generate TilesGenerated (Group.randomTenpaiGroups model.numberOfNonPairs) )

        SetNumberNonPairs num ->
            ( { model | numberOfNonPairs = num }, Cmd.none )

        TilesGenerated tiles ->
            ( { model | tiles = tiles, selectedWaits = Set.empty }, Cmd.none )

        ToggleWaitTile tile ->
            let
                compTile =
                    Tile.toComparable tile
            in
            if Set.member compTile model.selectedWaits then
                ( { model | selectedWaits = Set.remove compTile model.selectedWaits }, Cmd.none )

            else
                ( { model | selectedWaits = Set.insert compTile model.selectedWaits }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ renderNumberTilesSelector model
        , button [ class "button is-primary", onClick GenerateTiles ] [ Html.text "Generate " ]
        , UI.renderTiles False model.tiles
        , p [] [ Html.text "Select wait tiles:" ]
        , renderWaitButtons model

        -- , renderWinningTiles model
        ]


renderNumberTilesSelector : Model -> Html Msg
renderNumberTilesSelector model =
    let
        createRadioButton text numberOfNonPairs =
            let
                checked =
                    Html.Attributes.checked (model.numberOfNonPairs == numberOfNonPairs)
            in
            Html.label [ class "radio", onClick (SetNumberNonPairs numberOfNonPairs) ]
                [ Html.input [ type_ "radio", name "numGroups", checked ] [], Html.text text ]
    in
    div [ class "control" ]
        [ createRadioButton "4" 1
        , createRadioButton "7" 2
        , createRadioButton "10" 3
        , createRadioButton "13" 4
        ]


renderWaitButtons : Model -> Html Msg
renderWaitButtons model =
    let
        tileSuits =
            List.map .suit model.tiles
                |> List.Extra.unique
                |> List.sortBy Tile.suitToString

        selectedCss tile =
            if Set.member (Tile.toComparable tile) model.selectedWaits then
                class ""

            else
                style "opacity" "0.5"

        renderRow tiles =
            div [ class "is-flex is-flex-direction-row" ]
                (List.map
                    (\t ->
                        div
                            [ onClick (ToggleWaitTile t)
                            , selectedCss t
                            , style "cursor" "pointer"
                            ]
                            [ UI.drawTile t ]
                    )
                    tiles
                )
    in
    div []
        (List.map (\t -> renderRow (Tile.allSuitTiles t)) tileSuits)


renderWinningTiles : Model -> Html Msg
renderWinningTiles model =
    let
        winningTiles =
            Group.winningTiles model.tiles

        commonGroups =
            Group.commonGroups (List.map Tuple.second winningTiles)
    in
    table [ class "table is-striped is-fullwidth" ]
        [ tbody []
            (List.map
                (\( t, g ) ->
                    tr []
                        [ td [] [ UI.renderTiles False [ t ] ]
                        , td [] [ UI.drawGroups commonGroups g ]
                        ]
                )
                winningTiles
            )
        ]
