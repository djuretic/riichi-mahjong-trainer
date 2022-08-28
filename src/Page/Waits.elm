module Page.Waits exposing (Model, Msg, init, update, view)

import Group exposing (Group)
import Html exposing (Html, button, div, label, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import List.Extra
import Random
import Set exposing (Set)
import Tile exposing (Tile)
import UI


type alias Model =
    { suitSelection : SuitSelection
    , tiles : List Tile
    , waits : List ( Tile, List Group )
    , numberOfNonPairs : Int
    , minNumberOfWaits : Int
    , selectedWaits : Set Tile.ComparableTile
    , confirmedSelected : Bool
    }


type Msg
    = GenerateTiles
    | SetSuitSelection SuitSelection
    | SetNumberNonPairs Int
    | SetNumberMinWaits Int
    | TilesGenerated (List Tile)
    | ToggleWaitTile Tile
    | ConfirmSelected


type SuitSelection
    = RandomSuit
    | FixedSuitMan
    | FixedSuitPin
    | FixedSuitSou


init : ( Model, Cmd Msg )
init =
    ( { suitSelection = RandomSuit
      , tiles = []
      , waits = []
      , numberOfNonPairs = 1
      , minNumberOfWaits = 1
      , selectedWaits = Set.empty
      , confirmedSelected = False
      }
    , Random.generate TilesGenerated (Group.randomTenpaiGroups 1 Nothing)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, Random.generate TilesGenerated (Group.randomTenpaiGroups model.numberOfNonPairs (suitSelectionToSuit model.suitSelection)) )

        SetSuitSelection suitSelection ->
            ( { model | suitSelection = suitSelection }, Cmd.none )

        SetNumberNonPairs num ->
            ( { model | numberOfNonPairs = num }, Cmd.none )

        SetNumberMinWaits num ->
            ( { model | minNumberOfWaits = num }, Cmd.none )

        TilesGenerated tiles ->
            let
                waits =
                    Group.winningTiles tiles
            in
            if List.length waits < model.minNumberOfWaits then
                update GenerateTiles model

            else
                ( { model | tiles = tiles, waits = waits, selectedWaits = Set.empty, confirmedSelected = False }, Cmd.none )

        ToggleWaitTile tile ->
            let
                compTile =
                    Tile.toComparable tile
            in
            if Set.member compTile model.selectedWaits then
                ( { model | selectedWaits = Set.remove compTile model.selectedWaits }, Cmd.none )

            else
                ( { model | selectedWaits = Set.insert compTile model.selectedWaits }, Cmd.none )

        ConfirmSelected ->
            ( { model | confirmedSelected = True }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        renderLabel labelText content =
            div [ class "field is-horizontal" ]
                [ div [ class "field-label" ] [ label [ class "label" ] [ text labelText ] ]
                , div [ class "field-body" ] [ content ]
                ]

        suitSelector =
            renderLabel "Suit"
                (renderSuitSelection model)

        tilesSelector =
            renderLabel "Number of tiles"
                (renderNumberTilesSelector model)

        minWaitsSelector =
            renderLabel "Min. number of waits"
                (renderMinWaitsSelector model)
    in
    div []
        [ suitSelector
        , tilesSelector
        , minWaitsSelector
        , div [ class "field is-horizontal" ]
            [ div [ class "field-label" ] []
            , div [ class "field-body" ] [ div [ class "control" ] [ button [ class "button is-primary", onClick GenerateTiles ] [ text "Generate" ] ] ]
            ]
        , UI.renderTiles False model.tiles
        , p [] [ text "Select wait tiles:" ]
        , renderWaitButtons model
        , button [ class "button", onClick ConfirmSelected, disabled (Set.isEmpty model.selectedWaits) ] [ text "Confirm" ]
        , if model.confirmedSelected then
            renderWinningTiles model

          else
            text ""
        ]


renderSuitSelection : Model -> Html Msg
renderSuitSelection model =
    let
        createButton txt suitSel =
            let
                cssClass =
                    if model.suitSelection == suitSel then
                        class "button is-primary is-selected"

                    else
                        class "button"
            in
            button [ cssClass, onClick (SetSuitSelection suitSel) ] [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ createButton "Random" RandomSuit
        , createButton "Characters" FixedSuitMan
        , createButton "Circles" FixedSuitPin
        , createButton "Bamboos" FixedSuitSou
        ]


renderNumberTilesSelector : Model -> Html Msg
renderNumberTilesSelector model =
    let
        createButton txt numberOfNonPairs =
            let
                cssClass =
                    if model.numberOfNonPairs == numberOfNonPairs then
                        class "button is-primary is-selected"

                    else
                        class "button"
            in
            button [ cssClass, onClick (SetNumberNonPairs numberOfNonPairs) ] [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ createButton "4" 1
        , createButton "7" 2
        , createButton "10" 3
        , createButton "13" 4
        ]


renderMinWaitsSelector : Model -> Html Msg
renderMinWaitsSelector model =
    let
        createButton txt minNumberOfWaits =
            let
                cssClass =
                    if model.minNumberOfWaits == minNumberOfWaits then
                        class "button is-primary is-selected"

                    else
                        class "button"
            in
            button [ cssClass, onClick (SetNumberMinWaits minNumberOfWaits) ] [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ createButton "1" 1
        , createButton "2" 2
        , createButton "3" 3
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
        commonGroups =
            Group.commonGroups (List.map Tuple.second model.waits)
    in
    table [ class "table is-striped is-fullwidth" ]
        [ thead []
            [ th [] [ text "Tile" ]
            , th [] [ text "Groups" ]
            ]
        , tbody []
            (List.map
                (\( t, g ) ->
                    tr []
                        [ td [] [ UI.renderTiles False [ t ] ]
                        , td [] [ UI.drawGroups commonGroups g ]
                        ]
                )
                model.waits
            )
        ]


suitSelectionToSuit : SuitSelection -> Maybe Tile.Suit
suitSelectionToSuit suitSelection =
    case suitSelection of
        RandomSuit ->
            Nothing

        FixedSuitMan ->
            Just Tile.Man

        FixedSuitPin ->
            Just Tile.Pin

        FixedSuitSou ->
            Just Tile.Sou
