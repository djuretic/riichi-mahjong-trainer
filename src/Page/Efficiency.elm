module Page.Efficiency exposing (Model, Msg, init, update, view)

import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (class, classList, href, target)
import Html.Events exposing (onClick)
import I18n
import List.Extra
import Random
import Random.List
import Shanten
import Tile exposing (Tile)
import UI


type alias Model =
    { i18n : I18n.I18n
    , numberOfTiles : Int
    , tiles : List Tile
    , availableTiles : List Tile
    , discardedTiles : List Tile
    , shanten : Shanten.ShantenDetail
    , showShanten : Bool
    , tileAcceptance : Shanten.TileAcceptance
    , lastDiscardAcceptanceDetail : Maybe ( Tile, Shanten.TileAcceptanceDetail )
    , lastDiscardBestAcceptanceDetail : Maybe ( Tile, Shanten.TileAcceptanceDetail )
    }


type Msg
    = GenerateTiles
    | TilesGenerated ( List Tile, List Tile )
    | SetNumberOfTiles Int
    | DiscardTile Tile
    | DrawTile ( Maybe Tile, List Tile )
    | ToggleShowShanten


init : I18n.I18n -> ( Model, Cmd Msg )
init i18n =
    ( { i18n = i18n
      , numberOfTiles = 14
      , tiles = []
      , availableTiles = []
      , discardedTiles = []
      , shanten = Shanten.init
      , showShanten = False
      , tileAcceptance = Shanten.Draw Shanten.emptyTileAcceptanceDetail
      , lastDiscardAcceptanceDetail = Nothing
      , lastDiscardBestAcceptanceDetail = Nothing
      }
    , cmdGenerateTiles 14
    )


cmdGenerateTiles : Int -> Cmd Msg
cmdGenerateTiles numTiles =
    Random.generate TilesGenerated (Tile.randomList numTiles)


recalculateShanten : Model -> Model
recalculateShanten model =
    { model | shanten = Shanten.shanten model.tiles, tileAcceptance = Shanten.tileAcceptance model.discardedTiles model.tiles }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTiles ->
            ( model, cmdGenerateTiles model.numberOfTiles )

        TilesGenerated ( tiles, remainingTiles ) ->
            ( recalculateShanten
                { model
                    | tiles = Tile.sort tiles
                    , availableTiles = remainingTiles
                    , discardedTiles = []
                    , lastDiscardAcceptanceDetail = Nothing
                    , lastDiscardBestAcceptanceDetail = Nothing
                }
            , Cmd.none
            )

        SetNumberOfTiles numTiles ->
            update GenerateTiles { model | numberOfTiles = numTiles }

        DiscardTile tile ->
            if List.member tile model.tiles then
                let
                    filterTileAcceptance : Tile -> Shanten.TileAcceptance -> Maybe ( Tile, Shanten.TileAcceptanceDetail )
                    filterTileAcceptance tileToFind tileAccept =
                        case tileAccept of
                            Shanten.DiscardAndDraw tileAcceptanceList ->
                                List.Extra.find (\( t, _ ) -> t == tileToFind) tileAcceptanceList

                            _ ->
                                Nothing

                    bestTileAcceptance =
                        case model.tileAcceptance of
                            Shanten.DiscardAndDraw tileAcceptanceList ->
                                List.head tileAcceptanceList

                            _ ->
                                Nothing
                in
                ( { model
                    | tiles = List.Extra.remove tile model.tiles |> Tile.sort
                    , discardedTiles = model.discardedTiles ++ [ tile ]
                    , showShanten = False
                    , lastDiscardAcceptanceDetail = filterTileAcceptance tile model.tileAcceptance
                    , lastDiscardBestAcceptanceDetail = bestTileAcceptance
                  }
                , Random.generate DrawTile (Random.List.choose model.availableTiles)
                )

            else
                ( model, Cmd.none )

        DrawTile ( possibleTile, availableTiles ) ->
            case possibleTile of
                Just tile ->
                    ( recalculateShanten { model | tiles = model.tiles ++ [ tile ], availableTiles = availableTiles }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleShowShanten ->
            ( { model | showShanten = not model.showShanten }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        numberedTiles =
            False

        tilesString =
            Tile.listToString model.tiles

        uiMap uiMsg =
            case uiMsg of
                UI.TileOnClick tile ->
                    DiscardTile tile
    in
    div []
        [ div [ class "block" ]
            [ UI.label (I18n.numTilesSelectorTitle model.i18n) (numberTilesSelector model)
            ]
        , div [ class "block" ] [ UI.tilesDivWithOnClick model.i18n numberedTiles model.tiles |> Html.map uiMap ]
        , button [ class "button", onClick GenerateTiles ] [ text (I18n.newHandButton model.i18n) ]
        , button [ class "button", onClick ToggleShowShanten ] [ text "Shanten" ]
        , div [ classList [ ( "is-invisible", not model.showShanten ) ] ]
            [ text (String.fromInt model.shanten.final.shanten)
            , text "-shanten -> "
            , a [ href ("https://tenhou.net/2/?q=" ++ tilesString), target "_blank" ] [ text "Tenhou" ]
            , div [] (List.map (\lg -> UI.groupsSimple model.i18n numberedTiles lg) model.shanten.final.groups)
            , text "Tile acceptance"
            , tileAcceptance model
            ]
        , div [ classList [ ( "is-invisible", not model.showShanten ) ] ]
            [ text "Discard with widest tile acceptance"
            , Maybe.map (tileAcceptanceDiscardTile model numberedTiles) model.lastDiscardBestAcceptanceDetail |> Maybe.withDefault (span [] [])
            , text "Your discard"
            , Maybe.map (tileAcceptanceDiscardTile model numberedTiles) model.lastDiscardAcceptanceDetail |> Maybe.withDefault (span [] [])
            ]
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


tileAcceptance : Model -> Html Msg
tileAcceptance model =
    let
        addNumbers =
            False
    in
    case model.tileAcceptance of
        Shanten.Draw _ ->
            div [] []

        Shanten.DiscardAndDraw listAcceptance ->
            div []
                (List.map
                    (tileAcceptanceDiscardTile model addNumbers)
                    listAcceptance
                )


tileAcceptanceDiscardTile : Model -> Bool -> ( Tile, Shanten.TileAcceptanceDetail ) -> Html Msg
tileAcceptanceDiscardTile model addNumbers ( tile, detail ) =
    div UI.tilesDivAttrs
        ([ UI.tileSimple model.i18n addNumbers tile
         , text "->"
         ]
            ++ UI.tilesList model.i18n addNumbers detail.tiles
            ++ [ text (String.fromInt detail.numTiles)
               ]
        )
