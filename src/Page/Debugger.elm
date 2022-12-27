module Page.Debugger exposing (debugGroup, debugGroups, main)

import Browser
import Group exposing (Group)
import Html exposing (Html, a, button, div, input, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, href, placeholder, target, type_, value)
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
    , remainingTiles : List Tile
    , breakdown : Group.GroupsBreakdown
    , groups : List (List Group)
    , shanten : Shanten.ShantenDetail
    }


type Msg
    = HandStr String
    | Discard Tile
    | GenerateRandomTiles Int
    | TilesGenerated ( List Tile, List Tile )


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
      , shanten = Shanten.shanten []
      , groups = []
      , breakdown = Group.breakdownInit
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandStr handString ->
            ( model
                |> setTiles (Tile.fromString handString)
                |> setHandString handString
                |> calculateGroupsAndShantenFromTiles
            , Cmd.none
            )

        TilesGenerated ( tiles, remaining ) ->
            ( model
                |> setTiles (tiles |> Tile.sort)
                |> setRemainingTiles remaining
                |> calculateGroupsAndShantenFromTiles
            , Cmd.none
            )

        Discard tile ->
            let
                drawnTile =
                    List.Extra.uncons model.remainingTiles
            in
            case drawnTile of
                Just ( drawtile, remaining ) ->
                    ( model
                        |> setTiles (Tile.sort (List.Extra.remove tile model.tiles) ++ [ drawtile ])
                        |> setRemainingTiles remaining
                        |> calculateGroupsAndShantenFromTiles
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        GenerateRandomTiles numTiles ->
            ( model, Random.generate TilesGenerated (Tile.randomList numTiles) )


view : Model -> Html Msg
view model =
    let
        uiMap : UI.UIMsg -> Msg
        uiMap uiMsg =
            case uiMsg of
                UI.TileOnClick tile ->
                    Discard tile
    in
    div []
        [ input [ class "input", type_ "text", placeholder "Hand", value model.handString, onInput HandStr ] []
        , button [ class "button is-primary", onClick (GenerateRandomTiles 8) ] [ text "Random hand 8" ]
        , button [ class "button is-primary", onClick (GenerateRandomTiles 11) ] [ text "Random hand 11" ]
        , button [ class "button is-primary", onClick (GenerateRandomTiles 14) ] [ text "Random hand 14" ]
        , div [ class "block" ]
            [ UI.tilesWithOnClick model.i18n False model.tiles |> Html.map uiMap
            , a
                [ href ("https://tenhou.net/2/?q=" ++ (List.map Tile.toString model.tiles |> String.join ""))
                , target "_blank"
                ]
                [ text "Tenhou" ]
            ]
        , div [ class "block" ]
            [ p [] [ text ("Shanten: " ++ String.fromInt model.shanten.final.shanten) ]
            , p [] [ text ("Kokushi " ++ String.fromInt model.shanten.kokushi.shanten) ]
            , p [] [ text ("Chiitoitsu " ++ String.fromInt model.shanten.chiitoitsu.shanten) ]
            , debugGroups model.breakdown
            , p [] (List.map (UI.groups model.i18n False (Tile 1 Suit.Man)) model.shanten.final.groups)
            , p [] [ text "--" ]
            , p [] (List.map (UI.groups model.i18n False (Tile 1 Suit.Man)) model.groups)
            ]
        ]


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


setTiles : List Tile -> Model -> Model
setTiles tiles model =
    { model | tiles = tiles }


setRemainingTiles : List Tile -> Model -> Model
setRemainingTiles remainingTiles model =
    { model | remainingTiles = remainingTiles }


setHandString : String -> Model -> Model
setHandString handStr model =
    { model | handString = handStr }


calculateGroupsAndShantenFromTiles : Model -> Model
calculateGroupsAndShantenFromTiles model =
    let
        groupsBreakdown =
            Group.findGroups Group.FindPartials model.tiles

        groups =
            Group.breakdownCartesianProduct groupsBreakdown
    in
    { model | groups = groups, breakdown = groupsBreakdown, shanten = Shanten.shanten model.tiles }
