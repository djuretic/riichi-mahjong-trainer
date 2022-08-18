module Main exposing (main)

import Browser
import Group exposing (GroupType(..))
import Hand exposing (Yaku(..))
import Html
import Html.Attributes exposing (attribute)
import Page.Scoring
import Tile exposing (Suit(..))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


stylesheet : Html.Html Msg
stylesheet =
    let
        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "../css/bulma.min.css"
            ]
    in
    Html.node "link" attrs []


type alias Model =
    { page : Page
    , scoring : Page.Scoring.Model
    }


type Page
    = ScoringPage
    | WaitsPage


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( scoring, scoringCmd ) =
            Page.Scoring.init
    in
    ( { page = ScoringPage
      , scoring = scoring
      }
    , Cmd.map ScoringMsg scoringCmd
    )


type Msg
    = ScoringMsg Page.Scoring.Msg



-- | TenpaiHandGenerated Hand
-- | GenerateRandomTenpaiHand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScoringMsg smsg ->
            let
                ( scoring, scoringCmd ) =
                    Page.Scoring.update smsg model.scoring
            in
            ( { model | scoring = scoring }, Cmd.map ScoringMsg scoringCmd )



-- TenpaiHandGenerated hand ->
--     let
--         newHand =
--             Hand.count hand
--         allGroups =
--             Group.findGroups newHand.tiles
--     in
--     ( { model | handString = Hand.getHandString newHand, hand = newHand, allGroups = allGroups, guessedValue = guessValueInit }, Cmd.none )
-- GenerateRandomTenpaiHand ->
--     ( model
--     , Random.generate TenpaiHandGenerated Hand.randomTenpaiHand
--     )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view model =
    let
        content =
            case model.page of
                ScoringPage ->
                    Html.map ScoringMsg (Page.Scoring.view model.scoring)

                WaitsPage ->
                    Html.div [] []
    in
    Html.div [ Html.Attributes.class "container" ]
        [ stylesheet
        , Html.h1 [ Html.Attributes.class "title" ] [ Html.text "Riichi mahjong trainer" ]
        , content
        ]
