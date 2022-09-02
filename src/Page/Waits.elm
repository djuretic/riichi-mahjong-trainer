module Page.Waits exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Group exposing (Group)
import Html exposing (Html, button, div, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import List.Extra
import Point
import Random
import Set exposing (Set)
import Svg exposing (image, svg)
import Svg.Attributes exposing (filter, height, opacity, viewBox, width, x, xlinkHref, y)
import Tile exposing (Tile)
import Time
import UI exposing (renderTiles)


type alias Model =
    { suitSelection : SuitSelection
    , tiles : List Tile
    , waits : List ( Tile, List Group )
    , numberOfNonPairs : Int
    , minNumberOfWaits : Int
    , selectedWaits : Set Tile.ComparableTile
    , confirmedSelected : Bool
    , lastTick : Int
    , animatedTiles : List AnimatedTile
    }


type Msg
    = GenerateTiles
    | SetSuitSelection SuitSelection
    | SetNumberNonPairs Int
    | SetNumberMinWaits Int
    | TilesGenerated (List Tile)
    | ToggleWaitTile Tile
    | ConfirmSelected
    | StartWaitsAnimation ( Tile, List Group )
    | Tick Time.Posix


type SuitSelection
    = RandomSuit
    | FixedSuitMan
    | FixedSuitPin
    | FixedSuitSou


type alias AnimatedTile =
    { tile : Tile
    , state : AnimState
    , pos : Point.Point
    , next : List Point.Point
    }


type AnimState
    = TileInHand
    | WinningTileEnter
    | WinningTileExit


init : ( Model, Cmd Msg )
init =
    ( { suitSelection = RandomSuit
      , tiles = []
      , waits = []
      , numberOfNonPairs = 1
      , minNumberOfWaits = 1
      , selectedWaits = Set.empty
      , confirmedSelected = False
      , lastTick = 0
      , animatedTiles = []
      }
    , Random.generate TilesGenerated (Group.randomTenpaiGroups 1 Nothing)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.confirmedSelected then
        Browser.Events.onAnimationFrame Tick

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, Random.generate TilesGenerated (Group.randomTenpaiGroups model.numberOfNonPairs (suitSelectionToSuit model.suitSelection)) )

        SetSuitSelection suitSelection ->
            update GenerateTiles { model | suitSelection = suitSelection }

        SetNumberNonPairs num ->
            update GenerateTiles { model | numberOfNonPairs = num }

        SetNumberMinWaits num ->
            update GenerateTiles { model | minNumberOfWaits = num }

        TilesGenerated tiles ->
            let
                waits =
                    Group.winningTiles tiles
            in
            if List.length waits < model.minNumberOfWaits then
                update GenerateTiles model

            else
                ( initAnimatedTiles { model | tiles = tiles, waits = waits, selectedWaits = Set.empty, confirmedSelected = False }, Cmd.none )

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

        StartWaitsAnimation ( _, groups ) ->
            ( { model | animatedTiles = setupAnimation model groups }, Cmd.none )

        Tick tickTime ->
            let
                fps =
                    30

                fpsInterval =
                    1000 / fps

                now =
                    Time.posixToMillis tickTime

                elapsed =
                    now - model.lastTick
            in
            if model.lastTick == 0 then
                ( { model | lastTick = now }, Cmd.none )

            else if toFloat elapsed > fpsInterval then
                ( { model | lastTick = now - remainderBy (round fpsInterval) elapsed, animatedTiles = doAnimation model.animatedTiles }, Cmd.none )

            else
                ( model, Cmd.none )


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
        [ div [ class "block" ]
            [ suitSelector
            , tilesSelector
            , minWaitsSelector
            ]
        , div [ class "block" ] [ UI.renderTiles False model.tiles ]
        , div [ class "block" ] [ text "Select wait tiles:", renderWaitButtons model ]
        , button [ class "button block", onClick ConfirmSelected, disabled (Set.isEmpty model.selectedWaits) ] [ text "Confirm" ]
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
                            [ UI.drawTileSimple t ]
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
    div [ class "block" ]
        [ div [ class "block" ]
            [ text "Wait tiles:"
            , renderTiles False (List.map Tuple.first model.waits)
            ]
        , table [ class "table is-striped" ]
            [ thead []
                [ th [] [ text "Groups" ]
                ]
            , tbody []
                (List.map
                    (\( t, g ) ->
                        tr []
                            [ td [] [ UI.drawGroups commonGroups t g ]
                            ]
                    )
                    model.waits
                )
            ]
        , div [ class "block is-flex is-flex-direction-row" ]
            (List.map (\( t, g ) -> UI.drawTile [ onClick (StartWaitsAnimation ( t, g )), class "is-clickable" ] t) model.waits)
        , svg [ width "1000", height "120", viewBox "11 0 1000 120" ]
            (List.map
                (\at ->
                    let
                        ( posX, posY ) =
                            at.pos

                        cssClasses =
                            if List.member at.state [ WinningTileEnter, WinningTileExit ] then
                                filter "sepia(50%)"

                            else
                                filter ""

                        opacityNumber =
                            if at.state == WinningTileExit then
                                toFloat (posY + UI.tileHeight)
                                    / toFloat UI.tileHeight
                                    |> String.fromFloat

                            else
                                "1"
                    in
                    image [ cssClasses, xlinkHref (UI.tilePath at.tile), x (String.fromInt posX), y (String.fromInt posY), opacity opacityNumber ] []
                )
                model.animatedTiles
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


initAnimatedTiles : Model -> Model
initAnimatedTiles ({ tiles, waits } as model) =
    let
        baseTiles =
            List.indexedMap (\n t -> { tile = t, pos = ( n * UI.tileWidth, 0 ), next = [], state = TileInHand }) tiles

        waitTiles =
            List.map Tuple.first waits
                |> List.map (\t -> { tile = t, pos = ( 0, -UI.tileHeight ), next = [], state = WinningTileExit })
    in
    { model | animatedTiles = List.append baseTiles waitTiles }


setupAnimation : Model -> List Group -> List AnimatedTile
setupAnimation model groups =
    let
        dummyTile =
            Tile 0 Tile.Man

        groupTiles =
            List.map Group.toTiles groups
                |> List.intersperse [ dummyTile ]
                |> List.concat

        animTiles =
            resetNextMovements model.animatedTiles
    in
    List.foldl
        (\t ( offset, acc ) ->
            if t == dummyTile then
                ( offset + 15, acc )

            else
                ( offset + UI.tileWidth, updateAnimTile t offset acc )
        )
        ( 0, animTiles )
        groupTiles
        |> Tuple.second
        |> hideUnusedAnimatedTiles


updateAnimTile : Tile -> Int -> List AnimatedTile -> List AnimatedTile
updateAnimTile currentTile offsetX listAnimTiles =
    let
        tileIndex =
            List.Extra.findIndex (\t -> t.tile == currentTile && List.isEmpty t.next) listAnimTiles
    in
    case tileIndex of
        Just i ->
            List.Extra.updateAt i
                (\t ->
                    let
                        pos =
                            if t.state == WinningTileEnter || t.state == WinningTileExit then
                                Point.setX offsetX t.pos

                            else
                                t.pos
                    in
                    { t
                        | next = Point.easing pos ( offsetX, 0 )
                        , pos = pos
                        , state =
                            if t.state == WinningTileExit then
                                WinningTileEnter

                            else
                                t.state
                    }
                )
                listAnimTiles

        Nothing ->
            listAnimTiles


doAnimation : List AnimatedTile -> List AnimatedTile
doAnimation tiles =
    let
        process t =
            case t.next of
                [] ->
                    t

                pos :: xs ->
                    { t | pos = pos, next = xs }
    in
    List.map process tiles


resetNextMovements : List AnimatedTile -> List AnimatedTile
resetNextMovements tiles =
    List.map (\t -> { t | next = [] }) tiles


hideUnusedAnimatedTiles : List AnimatedTile -> List AnimatedTile
hideUnusedAnimatedTiles tiles =
    List.map
        (\t ->
            if List.isEmpty t.next && List.member t.state [ WinningTileEnter, WinningTileExit ] then
                { t | next = Point.easing t.pos ( Tuple.first t.pos, -UI.tileHeight ), state = WinningTileExit }

            else
                t
        )
        tiles
