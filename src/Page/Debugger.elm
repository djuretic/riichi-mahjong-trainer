module Page.Debugger exposing (..)

import Browser
import Group exposing (Group)
import Html exposing (Html, div, input, p, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onInput)
import I18n
import Shanten
import Suit
import Tile exposing (Tile)
import UI


type alias Model =
    { i18n : I18n.I18n
    , handString : String
    , tiles : List Tile
    , groups : List Group
    , shanten : Shanten.ShantenDetail
    }


type Msg
    = HandStr String


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
    ( { i18n = I18n.init I18n.En, handString = "", tiles = [], shanten = Shanten.shanten [], groups = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandStr handString ->
            let
                tiles =
                    Tile.fromString handString

                groups =
                    Group.findGroups Group.FindPartials tiles
                        |> Group.breakdownConcatMap (\g -> List.head g |> Maybe.withDefault [])
            in
            ( { model | handString = handString, tiles = tiles, shanten = Shanten.shanten tiles, groups = groups }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ class "input", type_ "text", placeholder "Hand", value model.handString, onInput HandStr ] []
        , p [] [ UI.tiles model.i18n True model.tiles ]
        , div [ class "block" ]
            [ p [] [ text (String.fromInt model.shanten.final.shanten) ]
            , p [] [ text ("Kokushi " ++ String.fromInt model.shanten.kokushi.shanten) ]
            , p [] [ text ("Chiitoitsu " ++ String.fromInt model.shanten.chiitoitsu.shanten) ]
            , UI.groups model.i18n False (Tile 1 Suit.Man) model.shanten.final.groups
            , p [] [ text "--" ]
            , UI.groups model.i18n False (Tile 1 Suit.Man) model.groups
            ]
        ]
