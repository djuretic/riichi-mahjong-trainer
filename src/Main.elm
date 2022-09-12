module Main exposing (main)

import Browser
import Html
import Html.Attributes exposing (attribute, class, href, style, target)
import Json.Encode as E
import Page.Scoring
import Page.Waits


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


linkStylesheetNode : String -> Html.Html msg
linkStylesheetNode path =
    let
        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" path
            ]
    in
    Html.node "link" attrs []


type alias Model =
    { page : Page
    , scoring : Page.Scoring.Model
    , waits : Page.Waits.Model
    }


type Page
    = ScoringPage
    | WaitsPage


init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        ( scoring, scoringCmd ) =
            Page.Scoring.init

        ( waits, waitsCmd ) =
            Page.Waits.init flags
    in
    ( { page = WaitsPage
      , scoring = scoring
      , waits = waits
      }
    , Cmd.batch [ Cmd.map ScoringMsg scoringCmd, Cmd.map WaitsMsg waitsCmd ]
    )


type Msg
    = SetPage Page
    | ScoringMsg Page.Scoring.Msg
    | WaitsMsg Page.Waits.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            ( { model | page = page }, Cmd.none )

        ScoringMsg smsg ->
            let
                ( scoring, scoringCmd ) =
                    Page.Scoring.update smsg model.scoring
            in
            ( { model | scoring = scoring }, Cmd.map ScoringMsg scoringCmd )

        WaitsMsg wmsg ->
            let
                ( waits, waitsCmd ) =
                    Page.Waits.update wmsg model.waits
            in
            ( { model | waits = waits }, Cmd.map WaitsMsg waitsCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.page == WaitsPage then
        Sub.map WaitsMsg (Page.Waits.subscriptions model.waits)

    else
        Sub.none


view : Model -> Html.Html Msg
view model =
    let
        content =
            case model.page of
                ScoringPage ->
                    Html.map ScoringMsg (Page.Scoring.view model.scoring)

                WaitsPage ->
                    Html.map WaitsMsg (Page.Waits.view model.waits)

        -- isActive targetPage =
        --     if model.page == targetPage then
        --         class "is-active"
        --     else
        --         class ""
    in
    Html.div
        [ style "background-color" "#ffffff"
        ]
        [ Html.div [ class "container" ]
            [ linkStylesheetNode "../css/bulma.min.css"
            , linkStylesheetNode "../css/app.css"

            -- , Html.nav [ class "navbar" ]
            --     [ Html.div [ class "navbar-menu" ]
            --         [ Html.div [ class "navbar-brand" ]
            --             [ Html.a [ class "navbar-item" ] [ Html.text "Mahjong" ] ]
            --         , Html.div [ class "navbar-start" ]
            --             [ Html.a [ class "navbar-item", onClick (SetPage ScoringPage), isActive ScoringPage ] [ Html.text "Scoring" ]
            --             , Html.a [ class "navbar-item", onClick (SetPage WaitsPage), isActive WaitsPage ] [ Html.text "Waits" ]
            --             ]
            --         ]
            --     ]
            , Html.h1 [ class "title" ] [ Html.text "Riichi mahjong trainer" ]
            , Html.div [ class "main" ] [ content ]
            ]
        , Html.footer [ class "footer" ]
            [ Html.div [ class "content has-text-centered" ]
                [ Html.p []
                    [ Html.text "Mahjong tile images by "
                    , Html.a [ href "https://www.martinpersson.org/", target "_blank" ] [ Html.text "Martin Persson" ]
                    ]
                ]
            ]
        ]
