port module Page.Waits exposing (Model, Msg(..), init, subscriptions, update, view)

import Anim
import Browser.Events
import FontAwesome.Regular as IconR
import FontAwesome.Solid as IconS
import Group exposing (Group)
import Html exposing (Html, a, button, div, li, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, href, style, target)
import Html.Events exposing (onClick)
import Html.Keyed
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
    , trainMode : TrainMode
    , singleSuitSelection : SingleSuitSelection
    , singleSuitSelectionAlt : SingleSuitSelection
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
    , keyboardInput : Set Int
    }


type GroupsView
    = GroupAnimation
    | GroupTable
    | ExternalLinks


type alias PreferencesModel =
    { trainMode : TrainMode
    , suitSelection : SingleSuitSelection
    , suitSelectionAlt : SingleSuitSelection
    , numberedTiles : Bool
    , numberOfNonPairs : Int
    , minNumberOfWaits : Int
    , groupsView : GroupsView
    }


type Msg
    = GenerateTiles Int
    | SetTrainMode TrainMode
    | SetSingleSuitSelection SingleSuitSelection
    | SetSingleSuitSelectionAlt SingleSuitSelection
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
    | KeyPressed String


type TrainMode
    = SingleSuit
    | TwoSuits


type SingleSuitSelection
    = RandomSuit
    | FixedSuitMan
    | FixedSuitPin
    | FixedSuitSou
    | FixedSuitHonor


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
                    { trainMode = SingleSuit
                    , suitSelection = RandomSuit
                    , suitSelectionAlt = RandomSuit
                    , numberOfNonPairs = 1
                    , minNumberOfWaits = 1
                    , groupsView = GroupAnimation
                    , numberedTiles = False
                    }

        model =
            clampMinNumberOfWaits
                { i18n = i18n
                , trainMode = prefs.trainMode
                , singleSuitSelection = prefs.suitSelection
                , singleSuitSelectionAlt = prefs.suitSelectionAlt
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
                , keyboardInput = Set.empty
                }
                |> avoidTwoEqualSuits True RandomSuit
    in
    ( model, cmdGenerateRandomTiles 0 model )


cmdGenerateRandomTiles : Int -> Model -> Cmd Msg
cmdGenerateRandomTiles numTries model =
    -- for efficiency we avoid the brute force method
    if model.trainMode == SingleSuit && model.numberOfNonPairs == 2 && model.minNumberOfWaits == 5 then
        Random.generate (TilesGenerated numTries) (Group.random5SidedWait (suitSelectionToPreference model.trainMode model.singleSuitSelection model.singleSuitSelectionAlt))

    else if model.trainMode == TwoSuits && model.numberOfNonPairs == 3 && model.minNumberOfWaits == 4 then
        Random.generate (TilesGenerated numTries) (Group.random4SidedWaitTwoSuits10Tiles (suitSelectionToPreference model.trainMode model.singleSuitSelection model.singleSuitSelectionAlt))

    else if model.trainMode == TwoSuits && numTries > 50 && model.numberOfNonPairs == 4 && model.minNumberOfWaits == 4 then
        -- because we don't know all the possible configurations for 4-tiled hands we give room to generate random ones until some tries
        Random.generate (TilesGenerated numTries) (Group.random4SidedWaitTwoSuits13Tiles (suitSelectionToPreference model.trainMode model.singleSuitSelection model.singleSuitSelectionAlt))

    else
        Random.generate (TilesGenerated numTries) (Group.randomTenpaiGroups model.numberOfNonPairs 30 (suitSelectionToPreference model.trainMode model.singleSuitSelection model.singleSuitSelectionAlt))


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyEvent =
            Browser.Events.onKeyUp (D.map KeyPressed (D.field "key" D.string))
    in
    if model.confirmedSelected then
        Sub.batch
            [ Browser.Events.onAnimationFrame Tick, keyEvent ]

    else
        keyEvent


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

        SetTrainMode suitSelection ->
            let
                newModel =
                    { model | trainMode = suitSelection }
            in
            update (GenerateTiles 0) (clampMinNumberOfWaits newModel |> avoidHonorSuitInSingleSuitMode)

        SetSingleSuitSelection suitSelection ->
            update (GenerateTiles 0) (avoidTwoEqualSuits True model.singleSuitSelection { model | singleSuitSelection = suitSelection })

        SetSingleSuitSelectionAlt suitSelectionAlt ->
            update (GenerateTiles 0) (avoidTwoEqualSuits False model.singleSuitSelectionAlt { model | singleSuitSelectionAlt = suitSelectionAlt })

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
            -- if numTries > 1000 then
            --     Debug.todo "Too many tries"
            if List.length waits < model.minNumberOfWaits then
                update (GenerateTiles (numTries + 1)) model

            else if model.trainMode == TwoSuits && Set.size waitSuits < 2 then
                update (GenerateTiles (numTries + 1)) model

            else
                -- let
                --     _ =
                --         Debug.log "numTries" numTries
                -- in
                ( initAnimatedTiles
                    { model
                        | tiles = tiles
                        , waits = waits
                        , selectedWaits = Set.empty
                        , confirmedSelected = False
                        , currentAnimatedTile = Nothing
                        , keyboardInput = Set.empty
                    }
                , Cmd.none
                )

        ToggleWaitTile tile ->
            if model.confirmedSelected || not (Tile.isValid tile) || not (List.member tile.suit (waitTileSuits model)) then
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
            ( Anim.tick tickTime doAnimation model, Cmd.none )

        UpdateI18n i18n ->
            ( { model | i18n = i18n }, Cmd.none )

        KeyPressed value ->
            let
                numValue =
                    String.toInt value

                stringValue =
                    String.toLower value

                suitValue =
                    Suit.fromString stringValue

                tileSuits =
                    waitTileSuits model
            in
            case numValue of
                Just val ->
                    case tileSuits of
                        [] ->
                            ( model, Cmd.none )

                        [ suit ] ->
                            update (ToggleWaitTile (Tile val suit)) model

                        _ ->
                            if Set.member val model.keyboardInput then
                                ( { model | keyboardInput = Set.remove val model.keyboardInput }, Cmd.none )

                            else
                                ( { model | keyboardInput = Set.insert val model.keyboardInput }, Cmd.none )

                Nothing ->
                    case suitValue of
                        Just suit ->
                            if not (Set.isEmpty model.keyboardInput) && List.length tileSuits > 1 then
                                -- toogle multiple tiles at once (eg: 112s)
                                Set.foldl (\n ( modl, _ ) -> update (ToggleWaitTile (Tile n suit)) modl) ( { model | keyboardInput = Set.empty }, Cmd.none ) model.keyboardInput

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            case stringValue of
                                "c" ->
                                    update ConfirmSelected model

                                "n" ->
                                    update (GenerateTiles 0) model

                                _ ->
                                    ( model, Cmd.none )


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

        suitSelector1Label =
            if model.trainMode == TwoSuits then
                I18n.suitSelectorTitleWithNumber "1" model.i18n

            else
                I18n.suitSelectorTitle model.i18n
    in
    div []
        [ div [ class "block" ]
            [ UI.label (I18n.trainWaitsMode model.i18n) (trainModeSelector model)
            , UI.label suitSelector1Label (singleSuitSelector model)
            , if model.trainMode == TwoSuits then
                UI.label (I18n.suitSelectorTitleWithNumber "2" model.i18n) (singleSuitSelectorAlt model)

              else
                div [] []
            , UI.label (I18n.numTilesSelectorTitle model.i18n) (numberTilesSelector model)
            , UI.label (I18n.minWaitsSelectorTitle model.i18n) (minWaitsSelector model)
            , UI.label (I18n.numberedTilesSelector model.i18n) (numberedTilesSelector model)
            ]
        , div [ class "block" ] [ UI.tilesDiv model.i18n model.numberedTiles model.tiles ]
        , div [ class "block" ]
            [ text (I18n.selectWaitTilesText model.i18n)
            , waitButtons model
            , div [ class "mt-3", classList [ ( "is-invisible", not model.confirmedSelected ) ] ] [ feedbackMsg ]
            ]
        , div [ class "block", classList [ ( "is-invisible", not model.confirmedSelected ) ] ] (winningTiles model)
        , div [ class "buttons" ]
            [ button
                [ class "button is-primary", onClick ConfirmSelected, disabled (Set.isEmpty model.selectedWaits || model.confirmedSelected) ]
                [ text (I18n.confirmTilesButton model.i18n) ]
            , button [ class "button", onClick (GenerateTiles 0) ] [ text (I18n.newHandButton model.i18n) ]
            ]
        , div [ class "block mb-5" ]
            (winningTilesSection model)
        ]


trainModeSelector : Model -> Html Msg
trainModeSelector model =
    let
        buttonUI txt suitSel =
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.trainMode == suitSel )
                    , ( "is-selected", model.trainMode == suitSel )
                    ]
                , onClick (SetTrainMode suitSel)
                ]
                [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ buttonUI (I18n.trainWaitsModeOne model.i18n) SingleSuit
        , buttonUI (I18n.trainWaitsModeTwo model.i18n) TwoSuits
        ]


singleSuitSelector : Model -> Html Msg
singleSuitSelector model =
    let
        buttonUI txt suitSel =
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
        [ buttonUI (I18n.suitSelectorTitleRandom model.i18n) RandomSuit
        , buttonUI (I18n.suitSelectorTitleMan model.i18n) FixedSuitMan
        , buttonUI (I18n.suitSelectorTitlePin model.i18n) FixedSuitPin
        , buttonUI (I18n.suitSelectorTitleSou model.i18n) FixedSuitSou
        , if model.trainMode == TwoSuits then
            buttonUI (I18n.suitSelectorTitleHonor model.i18n) FixedSuitHonor

          else
            text ""
        ]


singleSuitSelectorAlt : Model -> Html Msg
singleSuitSelectorAlt model =
    let
        buttonUI txt suitSel =
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.singleSuitSelectionAlt == suitSel )
                    , ( "is-selected", model.singleSuitSelectionAlt == suitSel )
                    ]
                , onClick (SetSingleSuitSelectionAlt suitSel)
                ]
                [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ buttonUI (I18n.suitSelectorTitleRandom model.i18n) RandomSuit
        , buttonUI (I18n.suitSelectorTitleMan model.i18n) FixedSuitMan
        , buttonUI (I18n.suitSelectorTitlePin model.i18n) FixedSuitPin
        , buttonUI (I18n.suitSelectorTitleSou model.i18n) FixedSuitSou
        , buttonUI (I18n.suitSelectorTitleHonor model.i18n) FixedSuitHonor
        ]


numberTilesSelector : Model -> Html Msg
numberTilesSelector model =
    let
        buttonUI txt numberOfNonPairs =
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
        [ buttonUI "4" 1
        , buttonUI "7" 2
        , buttonUI "10" 3
        , buttonUI "13" 4
        ]


minWaitsSelector : Model -> Html Msg
minWaitsSelector model =
    let
        buttonUI txt minNumberOfWaits =
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
        [ buttonUI "1" 1
        , buttonUI "2" 2
        , buttonUI "3" 3
        , buttonUI "4" 4
        , buttonUI "5" 5
        ]


numberedTilesSelector : Model -> Html Msg
numberedTilesSelector model =
    let
        buttonUI txt addNumbersToTiles =
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
        [ buttonUI (I18n.numberedTilesSelectorYes model.i18n) True
        , buttonUI (I18n.numberedTilesSelectorNo model.i18n) False
        ]


waitTileSuits : Model -> List Suit.Suit
waitTileSuits model =
    List.map .suit model.tiles
        |> List.Extra.unique
        |> List.sortBy Suit.toString


waitButtons : Model -> Html Msg
waitButtons model =
    let
        selectedCss tile =
            if Set.member (Tile.toComparable tile) model.selectedWaits then
                class ""

            else
                style "opacity" "0.5"

        addGhostTiles tiles htmlDivs =
            if List.length tiles == 7 then
                ( "g1", UI.backTile model.i18n [ class "is-invisible" ] ) :: List.append htmlDivs [ ( "g2", UI.backTile model.i18n [ class "is-invisible" ] ) ]

            else
                htmlDivs

        -- without Keyed, the ghost tiles will be shown as back tiles for a brief moment when changing suits
        renderRow tiles =
            Html.Keyed.node "div"
                [ class "waits-buttons is-flex is-flex-direction-row", UI.tileGapAttr ]
                (List.map
                    (\t ->
                        ( Tile.toString t
                        , UI.tile model.i18n
                            model.numberedTiles
                            [ onClick (ToggleWaitTile t)
                            , selectedCss t
                            , classList [ ( "is-clickable", not model.confirmedSelected ) ]
                            ]
                            t
                        )
                    )
                    tiles
                    |> addGhostTiles tiles
                )
    in
    div []
        (List.map (\t -> renderRow (Tile.allSuitTiles t)) (waitTileSuits model))


winningTilesSection : Model -> List (Html Msg)
winningTilesSection model =
    let
        groupGapSvg =
            15

        isActiveTabCss expected =
            classList [ ( "is-active", model.groupsView == expected ) ]

        noContentDiv =
            if model.confirmedSelected || model.groupsView == ExternalLinks then
                []

            else
                [ div [ class "block has-text-centered p-6" ] [ text (I18n.groupsContentPlaceholder model.i18n) ] ]

        groupsTable =
            if model.groupsView == GroupTable && model.confirmedSelected then
                [ div [ class "block is-flex is-flex-direction-column", style "gap" (String.fromInt groupGapSvg ++ "px") ]
                    (List.map
                        (\( t, g ) ->
                            div []
                                [ UI.groups model.i18n model.numberedTiles t g ]
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
                [ UI.backTile model.i18n [] ]

        groupsSvgAnimation =
            if model.groupsView == GroupAnimation && model.confirmedSelected then
                [ animationSvg groupGapSvg 1 "is-hidden-mobile" model
                , animationSvg groupGapSvg 0.8 "is-hidden-tablet" model
                , div [ class "tiles block is-flex is-flex-direction-row is-flex-wrap-wrap", UI.tileGapAttr ]
                    (resetAnimButton
                        :: List.map
                            (\( t, g ) ->
                                button [ class "button is-large animation-button pl-2 pr-2", classList [ ( "is-primary", model.currentAnimatedTile == Just t ) ], onClick (StartWaitsAnimation ( t, g )) ]
                                    [ UI.tile model.i18n model.numberedTiles [] t ]
                            )
                            model.waits
                    )
                ]

            else
                []

        externalLinks =
            if model.groupsView == ExternalLinks then
                [ div [ class "content" ]
                    [ ul []
                        [ li []
                            [ text "Common Wait Patterns (English) - "
                            , a [ href "https://drive.google.com/file/d/1K4NuE2UZgeqhSR-WsYWyQlRiEERh5VQo/view?usp=share_link", target "_blank" ] [ text "PNG" ]
                            , text " | "
                            , a [ href "https://drive.google.com/file/d/1MXvEAwT31RnTJzCqYTpfaL7bd7cmONjT/view?usp=share_link", target "_blank" ] [ text "PDF" ]
                            ]
                        , li []
                            [ text "Esperas Comunes de Mahjong (Español) - "
                            , a [ href "/img/Esperas_Comunes_Mahjong.png", target "_blank" ] [ text "PNG" ]
                            , text " | "
                            , a [ href "/pdf/Esperas_Comunes_Mahjong.pdf", target "_blank" ] [ text "PDF" ]
                            ]
                        , li []
                            [ text "Kutimaj Atendoj de Maĝango (Esperanto) - "
                            , a [ href "/img/Kutimaj_Atendoj_de_Maĝango.png", target "_blank" ] [ text "PNG" ]
                            , text " | "
                            , a [ href "/pdf/Kutimaj_Atendoj_de_Maĝango.pdf", target "_blank" ] [ text "PDF" ]
                            ]
                        ]
                    ]
                ]

            else
                []
    in
    div [ class "tabs is-boxed" ]
        [ ul []
            [ li
                [ isActiveTabCss GroupAnimation, onClick (SetGroupsView GroupAnimation) ]
                [ a [ target "_self" ] [ UI.icon "icon is-small" IconR.circlePlay, span [] [ text (I18n.animationTab model.i18n) ] ] ]
            , li
                [ isActiveTabCss GroupTable, onClick (SetGroupsView GroupTable) ]
                [ a [ target "_self" ] [ UI.icon "icon is-small" IconS.table, span [] [ text (I18n.tableTab model.i18n) ] ] ]
            , li
                [ isActiveTabCss ExternalLinks, onClick (SetGroupsView ExternalLinks) ]
                [ a [ target "_self" ] [ UI.icon "icon is-small" IconS.link, span [] [ text (I18n.linksTab model.i18n) ] ] ]
            ]
        ]
        :: (groupsTable
                ++ groupsSvgAnimation
                ++ externalLinks
                ++ noContentDiv
           )


winningTiles : Model -> List (Html Msg)
winningTiles model =
    [ text (I18n.showWaitTilesText model.i18n)
    , UI.tilesDiv model.i18n model.numberedTiles (List.map Tuple.first model.waits)
    ]


animationSvg : Int -> Float -> String -> Model -> Html Msg
animationSvg groupGapSvg zoom cssClass model =
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


suitSelectionToPreference : TrainMode -> SingleSuitSelection -> SingleSuitSelection -> Group.RandomSuitPreference
suitSelectionToPreference trainMode suitSelection suitSelectionAlt =
    let
        suitSelectionToSuit s =
            case s of
                RandomSuit ->
                    Suit.Man

                FixedSuitMan ->
                    Suit.Man

                FixedSuitPin ->
                    Suit.Pin

                FixedSuitSou ->
                    Suit.Sou

                FixedSuitHonor ->
                    Suit.Honor
    in
    case trainMode of
        SingleSuit ->
            case suitSelection of
                RandomSuit ->
                    Group.OneRandomSuit

                FixedSuitMan ->
                    Group.OneSuitFixed Suit.Man

                FixedSuitPin ->
                    Group.OneSuitFixed Suit.Pin

                FixedSuitSou ->
                    Group.OneSuitFixed Suit.Sou

                FixedSuitHonor ->
                    Group.OneRandomSuit

        -- we don't allow honors
        TwoSuits ->
            let
                noRandomOptions =
                    List.Extra.remove RandomSuit [ suitSelection, suitSelectionAlt ]
                        |> List.map suitSelectionToSuit
            in
            case noRandomOptions of
                [ s1, s2 ] ->
                    Group.TwoSuitsTwoFixed s1 s2

                [ s ] ->
                    Group.TwoSuitsOneFixed s

                _ ->
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


trainModeToString : TrainMode -> String
trainModeToString mode =
    case mode of
        SingleSuit ->
            "singleSuit"

        TwoSuits ->
            "twoSuits"


stringToTrainMode : String -> TrainMode
stringToTrainMode string =
    case string of
        "singleSuit" ->
            SingleSuit

        "twoSuits" ->
            TwoSuits

        _ ->
            SingleSuit


singleSuitSelectionToString : SingleSuitSelection -> String
singleSuitSelectionToString suitSel =
    case suitSel of
        RandomSuit ->
            "r"

        FixedSuitMan ->
            "m"

        FixedSuitPin ->
            "p"

        FixedSuitSou ->
            "s"

        FixedSuitHonor ->
            "z"


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

        "z" ->
            FixedSuitHonor

        _ ->
            RandomSuit


decoder : D.Decoder PreferencesModel
decoder =
    D.map7
        (\mode suit suitAlt nonPairs minWaits gView addNumbersToTiles ->
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
            { trainMode = stringToTrainMode (Maybe.withDefault "singleSuit" mode)
            , suitSelection = stringToSuitSelection suit
            , suitSelectionAlt = stringToSuitSelection (Maybe.withDefault "" suitAlt)
            , numberOfNonPairs = nonPairs
            , minNumberOfWaits = minWaits
            , groupsView = groupsView
            , numberedTiles = Maybe.withDefault False addNumbersToTiles
            }
        )
        (D.maybe (D.field "mode" D.string))
        (D.field "suit" D.string)
        (D.maybe (D.field "suitAlt" D.string))
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

                ExternalLinks ->
                    "l"
    in
    E.object
        [ ( "mode", E.string (trainModeToString model.trainMode) )
        , ( "suit", E.string (singleSuitSelectionToString model.singleSuitSelection) )
        , ( "suitAlt", E.string (singleSuitSelectionToString model.singleSuitSelectionAlt) )
        , ( "nonPairs", E.int model.numberOfNonPairs )
        , ( "minWaits", E.int model.minNumberOfWaits )
        , ( "groupsView", E.string groupsViewStr )
        , ( "tileNumbers", E.bool model.numberedTiles )
        ]


numWaitsUpperBound : Model -> Int
numWaitsUpperBound { numberOfNonPairs, trainMode } =
    case trainMode of
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
numWaitsLowerBound { trainMode } =
    case trainMode of
        SingleSuit ->
            1

        TwoSuits ->
            2


clampMinNumberOfWaits : Model -> Model
clampMinNumberOfWaits model =
    { model | minNumberOfWaits = clamp (numWaitsLowerBound model) (numWaitsUpperBound model) model.minNumberOfWaits }


avoidTwoEqualSuits : Bool -> SingleSuitSelection -> Model -> Model
avoidTwoEqualSuits isFromSuit1 prevSuitSelection model =
    if model.singleSuitSelection == model.singleSuitSelectionAlt && model.singleSuitSelection /= RandomSuit then
        -- avoid changing the option the user has just clicked in
        if isFromSuit1 then
            { model | singleSuitSelectionAlt = prevSuitSelection }

        else
            { model | singleSuitSelection = prevSuitSelection }

    else
        model


avoidHonorSuitInSingleSuitMode : Model -> Model
avoidHonorSuitInSingleSuitMode model =
    if model.trainMode == SingleSuit && model.singleSuitSelection == FixedSuitHonor then
        { model | singleSuitSelection = RandomSuit }

    else
        model
