port module Page.Waits exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Group exposing (Group)
import Html exposing (Html, a, button, div, label, li, text, ul)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Point
import Random
import Set exposing (Set)
import Svg exposing (image, svg)
import Svg.Attributes exposing (filter, height, opacity, viewBox, width, x, xlinkHref, y)
import Tile exposing (Tile)
import Time
import UI


port setStorageWaits : E.Value -> Cmd msg


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
    , groupsView : GroupsView
    }


type GroupsView
    = GroupAnimation
    | GroupTable


type alias PreferencesModel =
    { suitSelection : SuitSelection
    , numberOfNonPairs : Int
    , minNumberOfWaits : Int
    , groupsView : GroupsView
    }


type Msg
    = GenerateTiles
    | SetSuitSelection SuitSelection
    | SetNumberNonPairs Int
    | SetNumberMinWaits Int
    | TilesGenerated (List Tile)
    | ToggleWaitTile Tile
    | ConfirmSelected
    | SetGroupsView GroupsView
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


init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        prefs =
            case D.decodeValue decoder flags of
                Ok pref ->
                    pref

                Err _ ->
                    { suitSelection = RandomSuit, numberOfNonPairs = 1, minNumberOfWaits = 1, groupsView = GroupAnimation }

        model =
            { suitSelection = prefs.suitSelection
            , tiles = []
            , waits = []
            , numberOfNonPairs = prefs.numberOfNonPairs
            , minNumberOfWaits = prefs.minNumberOfWaits
            , selectedWaits = Set.empty
            , confirmedSelected = False
            , lastTick = 0
            , animatedTiles = []
            , groupsView = prefs.groupsView
            }
    in
    ( model, cmdGenerateRandomTiles model )


cmdGenerateRandomTiles : Model -> Cmd Msg
cmdGenerateRandomTiles model =
    Random.generate TilesGenerated (Group.randomTenpaiGroups model.numberOfNonPairs (suitSelectionToSuit model.suitSelection))


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
            ( model
            , Cmd.batch
                [ setStorageWaits (encode model)
                , cmdGenerateRandomTiles model
                ]
            )

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
            if model.confirmedSelected then
                ( model, Cmd.none )

            else
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

        SetGroupsView groupsView ->
            let
                newModel =
                    { model | groupsView = groupsView }
            in
            ( newModel, setStorageWaits (encode newModel) )

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
        , div [ class "block", classList [ ( "is-invisible", not model.confirmedSelected ) ] ] (renderWinningTiles model)
        , button [ class "button block", onClick ConfirmSelected, disabled (Set.isEmpty model.selectedWaits || model.confirmedSelected) ] [ text "Confirm" ]
        , div [ class "block" ]
            (renderWinningTilesSection model)
        ]


renderSuitSelection : Model -> Html Msg
renderSuitSelection model =
    let
        createButton txt suitSel =
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.suitSelection == suitSel )
                    , ( "is-selected", model.suitSelection == suitSel )
                    ]
                , onClick (SetSuitSelection suitSel)
                ]
                [ text txt ]
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
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.numberOfNonPairs == numberOfNonPairs )
                    , ( "is-selected", model.numberOfNonPairs == numberOfNonPairs )
                    ]
                , onClick (SetNumberNonPairs numberOfNonPairs)
                ]
                [ text txt ]
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
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.minNumberOfWaits == minNumberOfWaits )
                    , ( "is-selected", model.minNumberOfWaits == minNumberOfWaits )
                    ]
                , onClick (SetNumberMinWaits minNumberOfWaits)
                ]
                [ text txt ]
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
            div [ class "waits-buttons is-flex is-flex-direction-row", UI.tileGapCss ]
                (List.map
                    (\t ->
                        UI.drawTile
                            [ onClick (ToggleWaitTile t)
                            , selectedCss t
                            , style "cursor" "pointer"
                            ]
                            t
                    )
                    tiles
                )
    in
    div []
        (List.map (\t -> renderRow (Tile.allSuitTiles t)) tileSuits)


renderWinningTilesSection : Model -> List (Html Msg)
renderWinningTilesSection model =
    let
        groupGapSvg =
            15

        isActiveTabCss expected =
            classList [ ( "is-active", model.groupsView == expected ) ]

        noContentDiv =
            if model.confirmedSelected then
                []

            else
                [ div [ class "block has-text-centered p-6" ] [ text "Select waits to view possible groups" ] ]

        groupsTable =
            if model.groupsView == GroupTable && model.confirmedSelected then
                [ div [ class "block is-flex is-flex-direction-column", style "gap" (String.fromInt groupGapSvg ++ "px") ]
                    (List.map
                        (\( t, g ) ->
                            div []
                                [ UI.drawGroups t g ]
                        )
                        model.waits
                    )
                ]

            else
                []

        groupsSvgAnimation =
            if model.groupsView == GroupAnimation && model.confirmedSelected then
                [ div [ class "tiles block is-flex is-flex-direction-row", UI.tileGapCss, UI.tileHeightCss ]
                    (List.map (\( t, g ) -> UI.drawTile [ onClick (StartWaitsAnimation ( t, g )), class "is-clickable" ] t) model.waits)
                , renderSvg groupGapSvg model
                ]

            else
                []
    in
    div [ class "tabs is-boxed" ]
        [ ul []
            [ li [ isActiveTabCss GroupAnimation, onClick (SetGroupsView GroupAnimation) ] [ a [] [ text "Animation" ] ]
            , li [ isActiveTabCss GroupTable, onClick (SetGroupsView GroupTable) ] [ a [] [ text "Table" ] ]
            ]
        ]
        :: (groupsTable
                ++ groupsSvgAnimation
                ++ noContentDiv
           )


renderWinningTiles : Model -> List (Html Msg)
renderWinningTiles model =
    [ text "Wait tiles:"
    , UI.renderTiles False (List.map Tuple.first model.waits)
    ]


renderSvg : Int -> Model -> Html Msg
renderSvg groupGapSvg model =
    let
        heightStr =
            String.fromInt UI.tileHeight

        doubleHeightStr =
            String.fromInt (2 * UI.tileHeight)

        widthStr =
            String.fromInt
                ((model.numberOfNonPairs * 3 + 2) * (UI.tileWidth + UI.tileGap) + (groupGapSvg * 4) + 5)
    in
    div [ class "tiles block is-flex is-flex-direction-row", UI.tileHeightDoubleCss ]
        [ svg [ width widthStr, height doubleHeightStr, viewBox ("11 -" ++ heightStr ++ " " ++ widthStr ++ " " ++ doubleHeightStr) ]
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
                    image
                        [ cssClasses
                        , xlinkHref (UI.tilePath at.tile)
                        , x (String.fromInt posX)
                        , y (String.fromInt posY)
                        , width (String.fromInt (64 * UI.tileScale |> round))
                        , opacity opacityNumber
                        ]
                        []
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
            List.indexedMap (\n t -> { tile = t, pos = ( n * (UI.tileWidth + UI.tileGap), 0 ), next = [], state = TileInHand }) tiles

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
                ( offset + UI.tileWidth + UI.tileGap, updateAnimTile t offset acc )
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


suitSelectionToString : SuitSelection -> String
suitSelectionToString suitSel =
    case suitSel of
        RandomSuit ->
            "r"

        FixedSuitMan ->
            "m"

        FixedSuitPin ->
            "p"

        FixedSuitSou ->
            "s"


stringToSuitSelection : String -> SuitSelection
stringToSuitSelection s =
    case s of
        "r" ->
            RandomSuit

        "m" ->
            FixedSuitMan

        "p" ->
            FixedSuitPin

        "s" ->
            FixedSuitSou

        _ ->
            RandomSuit


decoder : D.Decoder PreferencesModel
decoder =
    D.map4
        (\suit nonPairs minWaits gView ->
            let
                groupsView =
                    case gView of
                        "a" ->
                            GroupAnimation

                        "t" ->
                            GroupTable

                        _ ->
                            GroupAnimation
            in
            { suitSelection = stringToSuitSelection suit, numberOfNonPairs = nonPairs, minNumberOfWaits = minWaits, groupsView = groupsView }
        )
        (D.field "suit" D.string)
        (D.field "nonPairs" D.int)
        (D.field "minWaits" D.int)
        (D.field "groupsView" D.string)


encode : Model -> E.Value
encode model =
    let
        groupsViewStr =
            case model.groupsView of
                GroupAnimation ->
                    "a"

                GroupTable ->
                    "t"
    in
    E.object
        [ ( "suit", E.string (suitSelectionToString model.suitSelection) )
        , ( "nonPairs", E.int model.numberOfNonPairs )
        , ( "minWaits", E.int model.minNumberOfWaits )
        , ( "groupsView", E.string groupsViewStr )
        ]
