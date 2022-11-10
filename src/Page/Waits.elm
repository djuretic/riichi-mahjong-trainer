port module Page.Waits exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Events
import FontAwesome.Regular as IconR
import FontAwesome.Solid as IconS
import Group exposing (Group)
import Html exposing (Html, a, button, div, li, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import I18n
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Point
import Random
import Set exposing (Set)
import Suit
import Svg exposing (image, svg)
import Svg.Attributes as SvgA
import Tile exposing (Tile)
import Time
import UI


port setStorageWaits : E.Value -> Cmd msg


type alias Model =
    { i18n : I18n.I18n
    , suitSelection : SuitSelection
    , singleSuitSelection : SingleSuitSelection
    , numberedTiles : Bool
    , tiles : List Tile
    , waits : List ( Tile, List Group )
    , numberOfNonPairs : Int
    , minNumberOfWaits : Int
    , selectedWaits : Set Tile.ComparableTile
    , confirmedSelected : Bool
    , lastTick : Int
    , animatedTiles : List AnimatedTile
    , currentAnimatedTile : Maybe Tile
    , groupsView : GroupsView
    }


type GroupsView
    = GroupAnimation
    | GroupTable


type alias PreferencesModel =
    { suitSelection : SingleSuitSelection
    , numberedTiles : Bool
    , numberOfNonPairs : Int
    , minNumberOfWaits : Int
    , groupsView : GroupsView
    }


type Msg
    = GenerateTiles Int
    | SetSuitSelection SuitSelection
    | SetSingleSuitSelection SingleSuitSelection
    | SetNumberNonPairs Int
    | SetNumberMinWaits Int
    | SetAddNumbersToTiles Bool
    | TilesGenerated Int (List Tile)
    | ToggleWaitTile Tile
    | ConfirmSelected
    | SetGroupsView GroupsView
    | StartWaitsAnimation ( Tile, List Group )
    | ResetWaitsAnimation
    | Tick Time.Posix
    | UpdateI18n I18n.I18n


type SuitSelection
    = SingleSuit
    | TwoSuits


type SingleSuitSelection
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


dummyTile : Tile
dummyTile =
    Tile 0 Suit.Man


init : I18n.I18n -> E.Value -> ( Model, Cmd Msg )
init i18n flags =
    let
        prefs =
            case D.decodeValue decoder flags of
                Ok pref ->
                    pref

                Err _ ->
                    { suitSelection = RandomSuit, numberOfNonPairs = 1, minNumberOfWaits = 1, groupsView = GroupAnimation, numberedTiles = False }

        model =
            clampMinNumberOfWaits
                { i18n = i18n
                , suitSelection = TwoSuits
                , singleSuitSelection = prefs.suitSelection
                , numberedTiles = prefs.numberedTiles
                , tiles = []
                , waits = []
                , numberOfNonPairs = prefs.numberOfNonPairs
                , minNumberOfWaits = prefs.minNumberOfWaits
                , selectedWaits = Set.empty
                , confirmedSelected = False
                , lastTick = 0
                , animatedTiles = []
                , currentAnimatedTile = Nothing
                , groupsView = prefs.groupsView
                }
    in
    ( model, cmdGenerateRandomTiles 0 model )


cmdGenerateRandomTiles : Int -> Model -> Cmd Msg
cmdGenerateRandomTiles numTries model =
    -- for efficiency we avoid the brute force method
    if model.suitSelection == SingleSuit && model.numberOfNonPairs == 2 && model.minNumberOfWaits == 5 then
        Random.generate (TilesGenerated numTries) (Group.random5SidedWait (suitSelectionToSuit SingleSuit model.singleSuitSelection))

    else if model.suitSelection == TwoSuits && model.numberOfNonPairs == 3 && model.minNumberOfWaits == 4 then
        Random.generate (TilesGenerated numTries) Group.random4SidedWaitTwoSuits

    else
        Random.generate (TilesGenerated numTries) (Group.randomTenpaiGroups model.numberOfNonPairs 30 (suitSelectionToSuit model.suitSelection model.singleSuitSelection))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.confirmedSelected then
        Browser.Events.onAnimationFrame Tick

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles numTries ->
            ( model
            , Cmd.batch
                [ setStorageWaits (encode model)
                , cmdGenerateRandomTiles numTries model
                ]
            )

        SetSuitSelection suitSelection ->
            let
                newModel =
                    { model | suitSelection = suitSelection }
            in
            update (GenerateTiles 0) (clampMinNumberOfWaits newModel)

        SetSingleSuitSelection suitSelection ->
            update (GenerateTiles 0) { model | singleSuitSelection = suitSelection }

        SetNumberNonPairs num ->
            let
                newModel =
                    { model | numberOfNonPairs = num }
            in
            update (GenerateTiles 0) (clampMinNumberOfWaits newModel)

        SetNumberMinWaits num ->
            update (GenerateTiles 0) { model | minNumberOfWaits = num }

        SetAddNumbersToTiles value ->
            let
                newModel =
                    { model | numberedTiles = value }
            in
            ( newModel, setStorageWaits (encode newModel) )

        TilesGenerated numTries tiles ->
            let
                waits =
                    Group.winningTiles tiles

                waitSuits =
                    List.map (\w -> Tuple.first w |> .suit |> Suit.toString) waits |> Set.fromList
            in
            if List.length waits < model.minNumberOfWaits then
                update (GenerateTiles (numTries + 1)) model

            else if model.suitSelection == TwoSuits && Set.size waitSuits < 2 then
                update (GenerateTiles (numTries + 1)) model

            else
                -- let
                --     _ =
                --         Debug.log "numTries" numTries
                -- in
                ( initAnimatedTiles { model | tiles = tiles, waits = waits, selectedWaits = Set.empty, confirmedSelected = False, currentAnimatedTile = Nothing }, Cmd.none )

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

        StartWaitsAnimation ( tile, groups ) ->
            let
                tilesWithDummySeparator =
                    List.map Group.toTiles groups
                        |> List.intersperse [ dummyTile ]
                        |> List.concat
            in
            ( { model | animatedTiles = setupAnimation model tilesWithDummySeparator, currentAnimatedTile = Just tile }, Cmd.none )

        ResetWaitsAnimation ->
            ( { model | animatedTiles = setupAnimation model model.tiles, currentAnimatedTile = Nothing }, Cmd.none )

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

        UpdateI18n i18n ->
            ( { model | i18n = i18n }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        expected =
            Set.fromList (List.map Tuple.first model.waits |> List.map Tile.toComparable)

        feedbackMsg =
            if expected == model.selectedWaits then
                span [ class "icon-text has-text-success" ]
                    [ UI.icon "icon" IconS.squareCheck
                    , span [] [ text (I18n.correctAnswer model.i18n) ]
                    ]

            else
                span [ class "icon-text has-text-danger" ]
                    [ UI.icon "icon" IconS.ban
                    , span [] [ text (I18n.wrongAnswer model.i18n) ]
                    ]
    in
    div []
        [ div [ class "block" ]
            [ UI.label (I18n.trainWaitsMode model.i18n) (renderSuitSelection model)
            , UI.label (I18n.suitSelectorTitle model.i18n) (renderSingleSuitSelection model)
            , UI.label (I18n.numTilesSelectorTitle model.i18n) (renderNumberTilesSelector model)
            , UI.label (I18n.minWaitsSelectorTitle model.i18n) (renderMinWaitsSelector model)
            , UI.label (I18n.numberedTilesSelector model.i18n) (renderNumberedTilesSelector model)
            ]
        , div [ class "block" ] [ UI.renderTiles model.i18n model.numberedTiles model.tiles ]
        , div [ class "block" ]
            [ text (I18n.selectWaitTilesText model.i18n)
            , renderWaitButtons model
            , div [ class "mt-3", classList [ ( "is-invisible", not model.confirmedSelected ) ] ] [ feedbackMsg ]
            ]
        , div [ class "block", classList [ ( "is-invisible", not model.confirmedSelected ) ] ] (renderWinningTiles model)
        , div [ class "buttons" ]
            [ button
                [ class "button is-primary", onClick ConfirmSelected, disabled (Set.isEmpty model.selectedWaits || model.confirmedSelected) ]
                [ text (I18n.confirmTilesButton model.i18n) ]
            , button [ class "button", onClick (GenerateTiles 0) ] [ text (I18n.newHandButton model.i18n) ]
            ]
        , div [ class "block mb-5" ]
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
        [ createButton (I18n.trainWaitsModeOne model.i18n) SingleSuit
        , createButton (I18n.trainWaitsModeTwo model.i18n) TwoSuits
        ]


renderSingleSuitSelection : Model -> Html Msg
renderSingleSuitSelection model =
    let
        createButton txt suitSel =
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.singleSuitSelection == suitSel )
                    , ( "is-selected", model.singleSuitSelection == suitSel )
                    ]
                , onClick (SetSingleSuitSelection suitSel)
                ]
                [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ createButton (I18n.suitSelectorTitleRandom model.i18n) RandomSuit
        , createButton (I18n.suitSelectorTitleMan model.i18n) FixedSuitMan
        , createButton (I18n.suitSelectorTitlePin model.i18n) FixedSuitPin
        , createButton (I18n.suitSelectorTitleSou model.i18n) FixedSuitSou
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
            if numWaitsUpperBound model < minNumberOfWaits then
                text ""

            else if minNumberOfWaits < numWaitsLowerBound model then
                text ""

            else
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
        , createButton "4" 4
        , createButton "5" 5
        ]


renderNumberedTilesSelector : Model -> Html Msg
renderNumberedTilesSelector model =
    let
        createButton txt addNumbersToTiles =
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.numberedTiles == addNumbersToTiles )
                    , ( "is-selected", model.numberedTiles == addNumbersToTiles )
                    ]
                , onClick (SetAddNumbersToTiles addNumbersToTiles)
                ]
                [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ createButton (I18n.numberedTilesSelectorYes model.i18n) True
        , createButton (I18n.numberedTilesSelectorNo model.i18n) False
        ]


renderWaitButtons : Model -> Html Msg
renderWaitButtons model =
    let
        tileSuits =
            List.map .suit model.tiles
                |> List.Extra.unique
                |> List.sortBy Suit.toString

        selectedCss tile =
            if Set.member (Tile.toComparable tile) model.selectedWaits then
                class ""

            else
                style "opacity" "0.5"

        renderRow tiles =
            div [ class "waits-buttons is-flex is-flex-direction-row", UI.tileGapCss ]
                (List.map
                    (\t ->
                        UI.drawTile model.i18n
                            model.numberedTiles
                            [ onClick (ToggleWaitTile t)
                            , selectedCss t
                            , classList [ ( "is-clickable", not model.confirmedSelected ) ]
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
                [ div [ class "block has-text-centered p-6" ] [ text (I18n.groupsContentPlaceholder model.i18n) ] ]

        groupsTable =
            if model.groupsView == GroupTable && model.confirmedSelected then
                [ div [ class "block is-flex is-flex-direction-column", style "gap" (String.fromInt groupGapSvg ++ "px") ]
                    (List.map
                        (\( t, g ) ->
                            div []
                                [ UI.drawGroups model.i18n model.numberedTiles t g ]
                        )
                        model.waits
                    )
                ]

            else
                []

        resetAnimButton =
            button
                [ class "button is-large animation-button pl-2 pr-2"
                , classList [ ( "is-primary", model.currentAnimatedTile == Nothing ) ]
                , onClick ResetWaitsAnimation
                ]
                [ UI.drawBackTile model.i18n ]

        groupsSvgAnimation =
            if model.groupsView == GroupAnimation && model.confirmedSelected then
                [ renderSvg groupGapSvg 1 "is-hidden-mobile" model
                , renderSvg groupGapSvg 0.8 "is-hidden-tablet" model
                , div [ class "tiles block is-flex is-flex-direction-row is-flex-wrap-wrap", UI.tileGapCss ]
                    (resetAnimButton
                        :: List.map
                            (\( t, g ) ->
                                button [ class "button is-large animation-button pl-2 pr-2", classList [ ( "is-primary", model.currentAnimatedTile == Just t ) ], onClick (StartWaitsAnimation ( t, g )) ]
                                    [ UI.drawTile model.i18n model.numberedTiles [] t ]
                            )
                            model.waits
                    )
                ]

            else
                []
    in
    div [ class "tabs is-boxed" ]
        [ ul []
            [ li
                [ isActiveTabCss GroupAnimation, onClick (SetGroupsView GroupAnimation) ]
                [ a [] [ UI.icon "icon is-small" IconR.circlePlay, span [] [ text (I18n.animationTab model.i18n) ] ] ]
            , li
                [ isActiveTabCss GroupTable, onClick (SetGroupsView GroupTable) ]
                [ a [] [ UI.icon "icon is-small" IconS.table, span [] [ text (I18n.tableTab model.i18n) ] ] ]
            ]
        ]
        :: (groupsTable
                ++ groupsSvgAnimation
                ++ noContentDiv
           )


renderWinningTiles : Model -> List (Html Msg)
renderWinningTiles model =
    [ text (I18n.showWaitTilesText model.i18n)
    , UI.renderTiles model.i18n model.numberedTiles (List.map Tuple.first model.waits)
    ]


renderSvg : Int -> Float -> String -> Model -> Html Msg
renderSvg groupGapSvg zoom cssClass model =
    let
        heightStr =
            String.fromInt UI.tileHeight

        doubleHeightStr =
            String.fromInt (2 * UI.tileHeight)

        widthPx =
            (model.numberOfNonPairs * 3 + 2) * (UI.tileWidth + UI.tileGap) + (groupGapSvg * 4) + 5

        svgWidth =
            toFloat widthPx * zoom |> round
    in
    div [ class ("tiles block is-flex is-flex-direction-row " ++ cssClass), style "min-width" "20px" ]
        [ svg [ SvgA.width (String.fromInt svgWidth), SvgA.viewBox ("0 -" ++ heightStr ++ " " ++ String.fromInt widthPx ++ " " ++ doubleHeightStr) ]
            (List.map
                (\at ->
                    let
                        ( posX, posY ) =
                            at.pos

                        cssClasses =
                            if List.member at.state [ WinningTileEnter, WinningTileExit ] then
                                SvgA.filter "sepia(50%)"

                            else
                                SvgA.filter ""

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
                        , SvgA.xlinkHref (UI.tilePath model.numberedTiles at.tile)
                        , SvgA.x (String.fromInt posX)
                        , SvgA.y (String.fromInt posY)
                        , SvgA.width (String.fromInt UI.tileWidth)
                        , SvgA.opacity opacityNumber
                        ]
                        [ Svg.title [] [ Svg.text (UI.tileTitle model.i18n at.tile) ] ]
                )
                model.animatedTiles
            )
        ]


suitSelectionToSuit : SuitSelection -> SingleSuitSelection -> Group.RandomSuitPreference
suitSelectionToSuit suitSelection singleSuitSelection =
    case suitSelection of
        SingleSuit ->
            case singleSuitSelection of
                RandomSuit ->
                    Group.OneRandomSuit

                FixedSuitMan ->
                    Group.OneSuit Suit.Man

                FixedSuitPin ->
                    Group.OneSuit Suit.Pin

                FixedSuitSou ->
                    Group.OneSuit Suit.Sou

        TwoSuits ->
            Group.TwoRandomSuits


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


{-| Tiles can contain dummyTile to indicate a separation between groups.
-}
setupAnimation : Model -> List Tile -> List AnimatedTile
setupAnimation model tiles =
    let
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
        tiles
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


suitSelectionToString : SingleSuitSelection -> String
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


stringToSuitSelection : String -> SingleSuitSelection
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
    D.map5
        (\suit nonPairs minWaits gView addNumbersToTiles ->
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
            { suitSelection = stringToSuitSelection suit
            , numberOfNonPairs = nonPairs
            , minNumberOfWaits = minWaits
            , groupsView = groupsView
            , numberedTiles = Maybe.withDefault False addNumbersToTiles
            }
        )
        (D.field "suit" D.string)
        (D.field "nonPairs" D.int)
        (D.field "minWaits" D.int)
        (D.field "groupsView" D.string)
        (D.maybe (D.field "tileNumbers" D.bool))


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
        [ ( "suit", E.string (suitSelectionToString model.singleSuitSelection) )
        , ( "nonPairs", E.int model.numberOfNonPairs )
        , ( "minWaits", E.int model.minNumberOfWaits )
        , ( "groupsView", E.string groupsViewStr )
        , ( "tileNumbers", E.bool model.numberedTiles )
        ]


numWaitsUpperBound : Model -> Int
numWaitsUpperBound { numberOfNonPairs, suitSelection } =
    case suitSelection of
        SingleSuit ->
            if numberOfNonPairs == 1 then
                3

            else
                5

        TwoSuits ->
            if numberOfNonPairs >= 3 then
                4

            else if numberOfNonPairs >= 2 then
                3

            else
                2


numWaitsLowerBound : Model -> Int
numWaitsLowerBound { suitSelection } =
    case suitSelection of
        SingleSuit ->
            1

        TwoSuits ->
            2


clampMinNumberOfWaits : Model -> Model
clampMinNumberOfWaits model =
    { model | minNumberOfWaits = clamp (numWaitsLowerBound model) (numWaitsUpperBound model) model.minNumberOfWaits }
