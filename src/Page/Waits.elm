module Page.Waits exposing (Model, Msg, init, update, view)

import Group exposing (Group)
import Hand
import Html exposing (Html, button, div, label, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import List.Extra
import Random
import Set exposing (Set)
import Tile exposing (Tile)
import UI


type alias Model =
    { handType : HandType
    , tiles : List Tile
    , waits : List ( Tile, List Group )
    , numberOfNonPairs : Int
    , selectedWaits : Set Tile.ComparableTile
    , confirmedSelected : Bool
    }


type Msg
    = GenerateTiles
    | SetHandType HandType
    | SetNumberNonPairs Int
    | TilesGenerated (List Tile)
    | NormalHandGenerated Hand.Hand
    | ToggleWaitTile Tile
    | ConfirmSelected


type HandType
    = NormalHand
    | SingleSuitHand


init : ( Model, Cmd Msg )
init =
    ( { handType = SingleSuitHand, tiles = [], waits = [], numberOfNonPairs = 1, selectedWaits = Set.empty, confirmedSelected = False }
    , Random.generate TilesGenerated (Group.randomTenpaiGroups 1)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            case model.handType of
                SingleSuitHand ->
                    ( model, Random.generate TilesGenerated (Group.randomTenpaiGroups model.numberOfNonPairs) )

                NormalHand ->
                    ( model, Random.generate NormalHandGenerated Hand.randomTenpaiHand )

        SetHandType handType ->
            ( { model | handType = handType }, Cmd.none )

        SetNumberNonPairs num ->
            ( { model | numberOfNonPairs = num }, Cmd.none )

        TilesGenerated tiles ->
            ( { model | tiles = tiles, selectedWaits = Set.empty, confirmedSelected = False }, Cmd.none )

        NormalHandGenerated hand ->
            ( { model | tiles = hand.tiles, selectedWaits = Set.empty, confirmedSelected = False }, Cmd.none )

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
            div [ class "field" ]
                [ label [ class "label" ] [ text labelText ]
                , content
                ]

        tilesSelector =
            if model.handType == SingleSuitHand then
                renderLabel "Number of tiles"
                    (renderNumberTilesSelector model)

            else
                div [] []
    in
    div []
        [ renderLabel "Hand type"
            (renderHandTypeSelector model)
        , tilesSelector
        , div [ class "field" ]
            [ div [ class "control" ] [ button [ class "button is-primary", onClick GenerateTiles ] [ text "Generate" ] ] ]
        , UI.renderTiles False model.tiles
        , p [] [ text "Select wait tiles:" ]
        , renderWaitButtons model
        , button [ class "button", onClick ConfirmSelected, disabled (Set.isEmpty model.selectedWaits) ] [ text "Confirm" ]
        , if model.confirmedSelected then
            renderWinningTiles model

          else
            text ""
        ]


renderHandTypeSelector : Model -> Html Msg
renderHandTypeSelector model =
    let
        cssClass handType =
            if model.handType == handType then
                class "button is-primary is-selected"

            else
                class "button"
    in
    div [ class "buttons has-addons" ]
        [ button [ cssClass SingleSuitHand, onClick (SetHandType SingleSuitHand) ] [ text "Single suit" ]
        , button [ cssClass NormalHand, onClick (SetHandType NormalHand) ] [ text "Normal" ]
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
                winningTiles
            )
        ]
