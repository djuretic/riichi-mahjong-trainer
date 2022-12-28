module Page.Debugger exposing (debugGroup, debugGroups, main)

import Browser
import Group exposing (Group)
import Html exposing (Html, a, button, div, input, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, disabled, href, placeholder, target, type_, value)
import Html.Events exposing (onClick, onInput)
import I18n
import List.Extra
import Random
import Shanten
import Suit
import Tile exposing (Tile)
import UI


type alias Model =
    { i18n : I18n.I18n
    , handString : String
    , tiles : List Tile
    , discards : List Tile
    , remainingTiles : List Tile
    , tileToDrawPosition : Int
    , breakdown : Group.GroupsBreakdown
    , groups : List (List Group)
    , shanten : Shanten.ShantenDetail

    -- last turn is first item
    , previousTurns : List Turn
    , showAnalysis : Bool
    }


type alias Turn =
    { tiles : List Tile
    , tileToDrawPosition : Int
    , discards : List Tile
    , shanten : Int
    }


type Msg
    = HandStr String
    | Discard Tile
    | GenerateRandomTiles Int
    | TilesGenerated ( List Tile, List Tile )
    | UndoTurn
    | SetShowAnalysis Bool


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = \m -> div [ class "container" ] [ view m ]
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { i18n = I18n.init I18n.En
      , handString = ""
      , tiles = []
      , remainingTiles = []
      , discards = []
      , tileToDrawPosition = 0
      , shanten = Shanten.shanten []
      , groups = []
      , breakdown = Group.breakdownInit
      , previousTurns = []
      , showAnalysis = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandStr handString ->
            ( model
                |> setTiles (Tile.fromString handString)
                |> resetTurns
                |> setDiscards []
                |> setTileToDrawPosition 0
                |> setHandString handString
                |> calculateGroupsAndShantenFromTiles
            , Cmd.none
            )

        TilesGenerated ( tiles, remaining ) ->
            ( model
                |> setTiles (tiles |> Tile.sort)
                |> resetTurns
                |> setDiscards []
                |> setTileToDrawPosition 0
                |> setRemainingTiles remaining
                |> calculateGroupsAndShantenFromTiles
            , Cmd.none
            )

        Discard tile ->
            let
                drawnTile =
                    List.Extra.getAt model.tileToDrawPosition model.remainingTiles
            in
            case drawnTile of
                Just drawtile ->
                    ( model
                        |> newTurnFromTilesAndDiscards
                        |> setTileToDrawPosition (model.tileToDrawPosition + 1)
                        |> setDiscards (model.discards ++ [ tile ])
                        |> setTiles (Tile.sort (List.Extra.remove tile model.tiles) ++ [ drawtile ])
                        |> calculateGroupsAndShantenFromTiles
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        GenerateRandomTiles numTiles ->
            ( model, Random.generate TilesGenerated (Tile.randomList numTiles) )

        UndoTurn ->
            case model.previousTurns of
                turn :: _ ->
                    ( model
                        |> setTiles turn.tiles
                        |> setTileToDrawPosition turn.tileToDrawPosition
                        |> setDiscards turn.discards
                        |> removeTurn
                        |> calculateGroupsAndShantenFromTiles
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SetShowAnalysis showAnalysis ->
            ( setShowAnalysis showAnalysis model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        uiMap : UI.UIMsg -> Msg
        uiMap uiMsg =
            case uiMsg of
                UI.TileOnClick tile ->
                    Discard tile

        analysisSection =
            if model.showAnalysis then
                div [ class "block" ]
                    [ turnsIndicator model
                    , p [] [ text ("Shanten: " ++ String.fromInt model.shanten.final.shanten) ]
                    , p [] [ text ("Kokushi " ++ String.fromInt model.shanten.kokushi.shanten) ]
                    , p [] [ text ("Chiitoitsu " ++ String.fromInt model.shanten.chiitoitsu.shanten) ]
                    , div [ class "block" ]
                        [ text "Tile acceptance:"
                        , tileAcceptanceSection (Shanten.tileAcceptance model.tiles) model
                        ]
                    , debugGroups model.breakdown
                    , p [] (List.map (UI.groups model.i18n False (Tile 1 Suit.Man)) model.shanten.final.groups)
                    , p [] [ text "--" ]
                    , p [] (List.map (UI.groups model.i18n False (Tile 1 Suit.Man)) model.groups)
                    , a
                        [ href ("https://tenhou.net/2/?q=" ++ (List.map Tile.toString model.tiles |> String.join ""))
                        , target "_blank"
                        ]
                        [ text "Tenhou" ]
                    ]

            else
                div [] []
    in
    div []
        [ input [ class "input", type_ "text", placeholder "Hand", value model.handString, onInput HandStr ] []
        , button [ class "button is-primary", onClick (GenerateRandomTiles 11) ] [ text "Random hand 11" ]
        , button [ class "button is-primary", onClick (GenerateRandomTiles 14) ] [ text "Random hand 14" ]
        , button [ class "button is-secondary", onClick UndoTurn, disabled (List.isEmpty model.previousTurns) ] [ text "Undo turn" ]
        , showAnalysisSelector model
        , div [ class "block" ]
            [ UI.tilesWithOnClick model.i18n False model.tiles |> Html.map uiMap
            , text "Discards:"
            , UI.tiles model.i18n False (List.take 6 model.discards)
            , UI.tiles model.i18n False (List.take 6 (List.drop 6 model.discards))
            , UI.tiles model.i18n False (List.drop 12 model.discards)
            ]
        , analysisSection
        ]


turnsIndicator : Model -> Html Msg
turnsIndicator model =
    div []
        (text "Turns"
            :: (List.reverse model.previousTurns
                    |> List.map (\t -> text (String.fromInt t.shanten))
                    |> List.intersperse (text "->")
               )
        )


tileAcceptanceSection : Shanten.TileAcceptance -> Model -> Html Msg
tileAcceptanceSection tileAcceptance model =
    case tileAcceptance of
        Shanten.Draw tiles ->
            UI.tiles model.i18n False tiles

        Shanten.DiscardAndDraw discardAndTiles ->
            div []
                (List.map
                    (\( discardTile, tiles ) ->
                        div []
                            [ UI.tileSimple model.i18n False discardTile
                            , text "->"
                            , UI.tiles model.i18n False tiles
                            ]
                    )
                    discardAndTiles
                )


debugGroup : List Group -> Html msg
debugGroup listGroup =
    if List.isEmpty listGroup then
        text "-"

    else
        ul [] (List.map (\g -> li [] [ text (Group.toString g) ]) listGroup)


debugGroups : Group.GroupsBreakdown -> Html msg
debugGroups groups =
    let
        generateTd l =
            List.map (\g -> td [] [ debugGroup g ]) l

        sevenPairsTxt =
            if List.isEmpty groups.chiitoitsu then
                "no"

            else
                "yes"
    in
    table [ class "table is-striped" ]
        [ thead []
            [ tr [] [ th [] [ text "groupsPerSuit" ] ] ]
        , tbody []
            [ tr [] (generateTd groups.perSuit.man)
            , tr [] (generateTd groups.perSuit.pin)
            , tr [] (generateTd groups.perSuit.sou)
            , tr [] (generateTd groups.perSuit.honor)
            , tr [] [ text ("Chiitoisu: " ++ sevenPairsTxt) ]
            ]
        ]


showAnalysisSelector : Model -> Html Msg
showAnalysisSelector model =
    let
        buttonUI txt showAnalysis =
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.showAnalysis == showAnalysis )
                    , ( "is-selected", model.showAnalysis == showAnalysis )
                    ]
                , onClick (SetShowAnalysis showAnalysis)
                ]
                [ text txt ]
    in
    div [ class "buttons has-addons" ]
        [ buttonUI (I18n.numberedTilesSelectorYes model.i18n) True
        , buttonUI (I18n.numberedTilesSelectorNo model.i18n) False
        ]


setTiles : List Tile -> Model -> Model
setTiles tiles model =
    { model | tiles = tiles }


setDiscards : List Tile -> Model -> Model
setDiscards discards model =
    { model | discards = discards }


resetTurns : Model -> Model
resetTurns model =
    { model | previousTurns = [] }


newTurnFromTilesAndDiscards : Model -> Model
newTurnFromTilesAndDiscards model =
    { model
        | previousTurns =
            { tiles = model.tiles
            , tileToDrawPosition = model.tileToDrawPosition
            , discards = model.discards
            , shanten = model.shanten.final.shanten
            }
                :: model.previousTurns
    }


removeTurn : Model -> Model
removeTurn model =
    { model | previousTurns = List.tail model.previousTurns |> Maybe.withDefault [] }


setTileToDrawPosition : Int -> Model -> Model
setTileToDrawPosition pos model =
    { model | tileToDrawPosition = pos }


setRemainingTiles : List Tile -> Model -> Model
setRemainingTiles remainingTiles model =
    { model | remainingTiles = remainingTiles }


setHandString : String -> Model -> Model
setHandString handStr model =
    { model | handString = handStr }


setShowAnalysis : Bool -> Model -> Model
setShowAnalysis showAnalysis model =
    { model | showAnalysis = showAnalysis }


calculateGroupsAndShantenFromTiles : Model -> Model
calculateGroupsAndShantenFromTiles model =
    let
        groupsBreakdown =
            Group.findGroups Group.FindPartials model.tiles

        groups =
            Group.breakdownCartesianProduct groupsBreakdown
    in
    { model | groups = groups, breakdown = groupsBreakdown, shanten = Shanten.shanten model.tiles }
