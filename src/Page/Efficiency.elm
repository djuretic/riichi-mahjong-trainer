port module Page.Efficiency exposing (Model, Msg(..), init, subscriptions, update, view)

import Anim
import Browser.Events
import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes as Attributes exposing (class, classList, href, style, target)
import Html.Events exposing (onClick)
import I18n
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Point
import Random
import Random.List
import Shanten
import Suit exposing (Suit)
import Svg exposing (image, svg)
import Svg.Attributes as SvgA
import Tile exposing (Tile)
import Time
import UI


port setStorageEfficiency : E.Value -> Cmd msg


type alias Model =
    { i18n : I18n.I18n
    , numberedTiles : Bool
    , numberOfTiles : Int
    , suits : List Suit
    , tiles : List Tile
    , availableTiles : List Tile
    , discardedTiles : List Tile
    , shanten : Shanten.ShantenDetail
    , tileAcceptance : Shanten.TileAcceptance
    , lastDiscardTiles : List Tile
    , lastDiscardTileAcceptance : List ( Tile, Shanten.TileAcceptanceDetail )
    , currentTab : Tab
    , animatedTiles : List AnimatedTile
    , lastTick : Int
    , tenpaiReached : Bool
    }


type alias PreferencesModel =
    { numberOfTiles : Int
    , suits : List Suit
    }


type Tab
    = CurrentHandAnalysisTab
    | LastMoveAnalysisTab


type Msg
    = GenerateTiles
    | TilesGenerated ( List Tile, List Tile )
    | SetNumberOfTiles Int
    | ToggleSuit Suit
    | DiscardTile Tile
    | VerifyTenpaiReached
    | DrawTile ( Maybe Tile, List Tile )
    | SetTab Tab
    | ShowHand ( Tile, List Tile )
    | UpdateI18n I18n.I18n
    | Tick Time.Posix
    | NoOp


type alias AnimatedTile =
    { tile : Tile
    , state : AnimState
    , pos : Point.Point
    , next : List Point.Point
    }


type AnimState
    = TileRemains
    | TileEnter
    | TileExit


init : I18n.I18n -> E.Value -> ( Model, Cmd Msg )
init i18n flags =
    let
        defaultSuits =
            [ Suit.Man, Suit.Pin, Suit.Sou ]

        prefs =
            case D.decodeValue decoder flags of
                Ok pref ->
                    pref

                Err _ ->
                    { numberOfTiles = 14, suits = defaultSuits }
    in
    ( { i18n = i18n
      , numberedTiles = False
      , numberOfTiles = prefs.numberOfTiles
      , suits = prefs.suits
      , tiles = []
      , availableTiles = []
      , discardedTiles = []
      , shanten = Shanten.init
      , tileAcceptance = Shanten.Draw Shanten.emptyTileAcceptanceDetail
      , lastDiscardTiles = []
      , lastDiscardTileAcceptance = []
      , currentTab = LastMoveAnalysisTab
      , animatedTiles = []
      , lastTick = 0
      , tenpaiReached = False
      }
    , cmdGenerateTiles prefs.numberOfTiles prefs.suits
    )


cmdGenerateTiles : Int -> List Suit -> Cmd Msg
cmdGenerateTiles numTiles suits =
    Random.generate TilesGenerated (Tile.randomListOfSuits numTiles suits)


recalculateShanten : Model -> Model
recalculateShanten model =
    { model | shanten = Shanten.shanten model.tiles, tileAcceptance = Shanten.tileAcceptance model.discardedTiles model.tiles }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model
            , Cmd.batch
                [ setStorageEfficiency (encode model)
                , cmdGenerateTiles model.numberOfTiles model.suits
                ]
            )

        TilesGenerated ( tiles, remainingTiles ) ->
            let
                sortedTiles =
                    Tile.sort tiles

                shanten =
                    Shanten.shanten sortedTiles
            in
            if shanten.final.shanten < 0 then
                update GenerateTiles model

            else
                -- TODO don't recalculate shanten twice
                ( recalculateShanten
                    { model
                        | tiles = sortedTiles
                        , availableTiles = remainingTiles
                        , discardedTiles = []
                        , lastDiscardTiles = []
                        , lastDiscardTileAcceptance = []
                        , animatedTiles = []
                        , tenpaiReached = False
                    }
                , Cmd.none
                )

        SetNumberOfTiles numTiles ->
            update GenerateTiles { model | numberOfTiles = numTiles }

        ToggleSuit suit ->
            let
                suits =
                    if List.member suit model.suits then
                        List.Extra.remove suit model.suits

                    else
                        suit :: model.suits
            in
            if List.isEmpty suits then
                ( model, Cmd.none )

            else
                update GenerateTiles { model | suits = suits }

        DiscardTile tile ->
            let
                alreadyWon =
                    model.shanten.final.shanten < 0
            in
            if List.member tile model.tiles && not (List.isEmpty model.availableTiles) && not alreadyWon && not model.tenpaiReached then
                let
                    lastDiscardAcceptance =
                        case model.tileAcceptance of
                            Shanten.DiscardAndDraw tileAcceptanceList ->
                                tileAcceptanceList

                            _ ->
                                []
                in
                { model
                    | tiles = List.Extra.remove tile model.tiles |> Tile.sort
                    , discardedTiles = model.discardedTiles ++ [ tile ]
                    , lastDiscardTiles = model.tiles
                    , lastDiscardTileAcceptance = lastDiscardAcceptance
                }
                    |> initAnimatedTiles
                    |> update VerifyTenpaiReached

            else
                ( model, Cmd.none )

        VerifyTenpaiReached ->
            let
                modelWithRecalculatedShanten =
                    if model.shanten.final.shanten == 0 then
                        recalculateShanten model

                    else
                        model
            in
            if modelWithRecalculatedShanten.shanten.final.shanten == 0 then
                ( { model | tenpaiReached = True }, Cmd.none )

            else
                ( model, Random.generate DrawTile (Random.List.choose model.availableTiles) )

        DrawTile ( possibleTile, availableTiles ) ->
            case possibleTile of
                Just tile ->
                    ( recalculateShanten { model | tiles = model.tiles ++ [ tile ], availableTiles = availableTiles }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetTab tab ->
            ( { model | currentTab = tab }, Cmd.none )

        ShowHand ( tiles, drawnTile ) ->
            -- TODO complete
            ( model, Cmd.none )

        UpdateI18n i18n ->
            ( { model | i18n = i18n }, Cmd.none )

        Tick tickTime ->
            ( Anim.tick tickTime doAnimation model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        tilesString =
            Tile.listToString model.tiles

        tenpaiMessage =
            if model.tenpaiReached then
                "Tenpai reached!"

            else
                "-"

        uiMap uiMsg =
            case uiMsg of
                UI.TileOnClick tile ->
                    DiscardTile tile

        uiMapLastHand uiMsg =
            case uiMsg of
                UI.TileOnClick _ ->
                    NoOp

        isGoodTileToDiscard : Tile.Tile -> Bool
        isGoodTileToDiscard tile =
            List.member tile (List.map Tuple.first model.lastDiscardTileAcceptance) && (List.length model.lastDiscardTileAcceptance /= List.length (List.Extra.unique model.lastDiscardTiles))

        tabs =
            div [ class "tabs is-boxed" ]
                [ ul []
                    [ li
                        [ classList [ ( "is-active", model.currentTab == LastMoveAnalysisTab ) ], onClick (SetTab LastMoveAnalysisTab) ]
                        [ a [ target "_self" ] [ text "Last move" ] ]
                    , li
                        [ classList [ ( "is-active", model.currentTab == CurrentHandAnalysisTab ) ], onClick (SetTab CurrentHandAnalysisTab) ]
                        [ a [ target "_self" ] [ text "Hand" ] ]
                    ]
                ]
    in
    div []
        [ div [ class "block" ]
            [ UI.label (I18n.numTilesSelectorTitle model.i18n) (numberTilesSelector model)
            , UI.label (I18n.suitSelectorTitle model.i18n) (suitSelector model)
            ]
        , div [ class "block" ]
            [ text tenpaiMessage
            , UI.tilesDivWithOnClick model.i18n model.numberedTiles model.tiles |> Html.map uiMap
            , text ("Remaining pieces: " ++ (String.fromInt <| List.length model.availableTiles))
            ]
        , button [ class "button", onClick GenerateTiles ] [ text (I18n.newHandButton model.i18n) ]
        , tabs
        , div [ classList [ ( "is-hidden", model.currentTab /= CurrentHandAnalysisTab ) ] ]
            [ tenhouLink model tilesString
            , div [] (List.map (\lg -> UI.groupsSimple model.i18n model.numberedTiles lg) (model.shanten.final.groupConfigurations |> List.map .groups))
            , text "Tile acceptance"
            , tileAcceptance model
            ]
        , div [ classList [ ( "is-hidden", model.currentTab /= LastMoveAnalysisTab ) ] ]
            [ if List.isEmpty model.discardedTiles then
                div [] []

              else
                tenhouLink model (Tile.listToString model.lastDiscardTiles)

            -- , animationSvg groupGapSvg(15) 1 "is-hidden-mobile" model
            , UI.tilesDivWithOnClickAndAttrs model.i18n
                model.numberedTiles
                (\t ->
                    if isGoodTileToDiscard t then
                        [ Attributes.style "filter" "sepia(50%)" ]

                    else
                        []
                )
                model.lastDiscardTiles
                |> Html.map uiMapLastHand
            , text "Discards"
            , UI.tilesDiv model.i18n model.numberedTiles model.discardedTiles
            , text "Tile acceptance"
            , div [ class "block" ]
                (List.map
                    (tileAcceptanceDiscardTile model)
                    model.lastDiscardTileAcceptance
                )
            ]
        ]


initAnimatedTiles : Model -> Model
initAnimatedTiles ({ lastDiscardTileAcceptance, discardedTiles } as model) =
    if List.length discardedTiles == 1 then
        let
            baseTiles =
                List.indexedMap
                    (\n ( t, _ ) ->
                        { tile = t
                        , pos = ( 0, n * (UI.tileHeight + UI.tileGap) )
                        , next = []
                        , state = TileRemains
                        }
                    )
                    lastDiscardTileAcceptance
        in
        { model | animatedTiles = baseTiles }

    else
        -- setup animation
        let
            toDiscardTiles =
                List.map Tuple.first lastDiscardTileAcceptance

            prevDiscardTiles =
                List.map .tile model.animatedTiles

            differenceBetweenLists : List Tile -> List Tile -> ( List Tile, List Tile, List Tile )
            differenceBetweenLists list1 list2 =
                let
                    ( l1, l2 ) =
                        List.partition (\t -> List.member t list2) list1

                    ( _, l4 ) =
                        List.partition (\t -> List.member t list1) list2
                in
                ( l2, l1, l4 )

            ( tilesToExit, tilesToRemain, tilesToEnter ) =
                differenceBetweenLists prevDiscardTiles toDiscardTiles
        in
        { model
            | animatedTiles =
                setupAnimationRemain toDiscardTiles tilesToRemain model.animatedTiles
                    |> setupAnimationExit tilesToExit
                    |> setupAnimationEnter toDiscardTiles tilesToEnter
        }


setupAnimationRemain : List Tile -> List Tile -> List AnimatedTile -> List AnimatedTile
setupAnimationRemain toDiscardTiles tilesToRemain animatedTiles =
    List.map
        (\at ->
            if List.member at.tile tilesToRemain then
                let
                    index =
                        List.Extra.elemIndex at.tile toDiscardTiles |> Maybe.withDefault -1
                in
                { at | state = TileRemains, next = Point.easing at.pos ( 0, index * (UI.tileHeight + UI.tileGap) ) }

            else
                at
        )
        animatedTiles


setupAnimationExit : List Tile -> List AnimatedTile -> List AnimatedTile
setupAnimationExit tilesToExit animatedTiles =
    List.map
        (\at ->
            if List.member at.tile tilesToExit then
                { at | state = TileExit, next = Point.easing at.pos ( -100, Tuple.second at.pos ) }

            else
                at
        )
        animatedTiles


setupAnimationEnter : List Tile -> List Tile -> List AnimatedTile -> List AnimatedTile
setupAnimationEnter toDiscardTiles tilesToEnter animatedTiles =
    let
        enterTiles =
            List.map
                (\t ->
                    let
                        index =
                            List.Extra.elemIndex t toDiscardTiles |> Maybe.withDefault -1

                        pos =
                            ( -100, index * (UI.tileHeight + UI.tileGap) )
                    in
                    { tile = t
                    , pos = pos
                    , next = Point.easing pos ( 0, Tuple.second pos )
                    , state = TileEnter
                    }
                )
                tilesToEnter
    in
    animatedTiles ++ enterTiles



-- TODO remove tiles that are not visible and exiting


doAnimation : List AnimatedTile -> List AnimatedTile
doAnimation tiles =
    let
        process : AnimatedTile -> AnimatedTile
        process t =
            case t.next of
                [] ->
                    t

                pos :: xs ->
                    { t | pos = pos, next = xs }
    in
    List.map process tiles


animationSvg : Int -> Float -> String -> Model -> Html Msg
animationSvg groupGapSvg zoom cssClass model =
    let
        totalHeightStr =
            String.fromInt (10 * UI.tileHeight)

        widthPx =
            (model.numberOfTiles + 2) * (UI.tileWidth + UI.tileGap) + (groupGapSvg * 4) + 5

        svgWidth =
            toFloat widthPx * zoom |> round
    in
    div [ class ("tiles block is-flex is-flex-direction-row " ++ cssClass), style "min-width" "20px" ]
        [ svg [ SvgA.width (String.fromInt svgWidth), SvgA.viewBox ("0 0 " ++ String.fromInt widthPx ++ " " ++ totalHeightStr) ]
            (List.map
                (\animTile ->
                    let
                        ( posX, posY ) =
                            animTile.pos
                    in
                    image
                        [ SvgA.x (String.fromInt posX)
                        , SvgA.y (String.fromInt posY)
                        , SvgA.width (String.fromInt UI.tileWidth)
                        , SvgA.height (String.fromInt UI.tileHeight)
                        , SvgA.xlinkHref (UI.tilePath model.numberedTiles animTile.tile)
                        ]
                        [ Svg.title [] [ Svg.text (UI.tileTitle model.i18n animTile.tile) ] ]
                )
                model.animatedTiles
            )
        ]


tenhouLink : Model -> String -> Html Msg
tenhouLink model tilesString =
    div []
        [ text (String.fromInt model.shanten.final.shanten)
        , text "-shanten -> "
        , a [ href ("https://tenhou.net/2/?q=" ++ tilesString), target "_blank" ] [ text "Tenhou" ]
        ]


numberTilesSelector : Model -> Html Msg
numberTilesSelector model =
    let
        buttonUI txt numberOfTiles =
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.numberOfTiles == numberOfTiles )
                    , ( "is-selected", model.numberOfTiles == numberOfTiles )
                    ]
                , onClick (SetNumberOfTiles numberOfTiles)
                ]
                [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ buttonUI "5" 5
        , buttonUI "8" 8
        , buttonUI "11" 11
        , buttonUI "14" 14
        ]


suitSelector : Model -> Html Msg
suitSelector model =
    let
        buttonUI txt suitSel =
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", List.member suitSel model.suits )
                    , ( "is-selected", List.member suitSel model.suits )
                    ]
                , onClick (ToggleSuit suitSel)
                ]
                [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ buttonUI (I18n.suitSelectorTitleMan model.i18n) Suit.Man
        , buttonUI (I18n.suitSelectorTitlePin model.i18n) Suit.Pin
        , buttonUI (I18n.suitSelectorTitleSou model.i18n) Suit.Sou
        , buttonUI (I18n.suitSelectorTitleHonor model.i18n) Suit.Honor
        ]


tileAcceptance : Model -> Html Msg
tileAcceptance model =
    case model.tileAcceptance of
        Shanten.Draw _ ->
            div [] []

        Shanten.DiscardAndDraw listAcceptance ->
            div []
                (List.map
                    (tileAcceptanceDiscardTile model)
                    listAcceptance
                )


tileAcceptanceDiscardTile : Model -> ( Tile, Shanten.TileAcceptanceDetail ) -> Html Msg
tileAcceptanceDiscardTile model ( tile, detail ) =
    let
        uiMap : UI.UIMsg -> Msg
        uiMap uiMsg =
            case uiMsg of
                UI.TileOnClick clickedTile ->
                    -- TODO prev turn tiles
                    ShowHand ( clickedTile, model.tiles )

        lastTileCss =
            List.Extra.last model.discardedTiles
                |> Maybe.map
                    (\t ->
                        if t == tile then
                            class "discarded-tile-last-turn"

                        else
                            class ""
                    )
                |> Maybe.withDefault (class "")
    in
    div (lastTileCss :: UI.tilesDivAttrs)
        ([ UI.tileSimple model.i18n model.numberedTiles tile
         , text "->"
         ]
            ++ (UI.tilesListWithOnClick model.i18n model.numberedTiles detail.tiles |> List.map (Html.map uiMap))
            ++ [ text (String.fromInt detail.numTiles)
               ]
        )



-- decoder for PreferencesModel


decoder : D.Decoder PreferencesModel
decoder =
    D.map2
        (\numTiles suits ->
            { numberOfTiles = numTiles
            , suits = List.filterMap Suit.fromString suits
            }
        )
        (D.field "numTiles" D.int)
        (D.field "suits" (D.list D.string))


encode : Model -> E.Value
encode model =
    E.object
        [ ( "numTiles", E.int model.numberOfTiles )
        , ( "suits", E.list E.string (List.map Suit.toString model.suits) )
        ]
