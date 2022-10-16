port module Main exposing (main)

-- import Page.Scoring

import Browser
import FontAwesome
import FontAwesome.Brands as Brands
import FontAwesome.Solid as Solid
import Html
import Html.Attributes exposing (class, href, target, title)
import Html.Events exposing (onClick)
import I18n
import Json.Decode as D
import Json.Encode as E
import Page.Waits
import UI


port setDarkMode : String -> Cmd msg


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { i18n : I18n.I18n
    , page : Page
    , theme : Theme

    -- , scoring : Page.Scoring.Model
    , waits : Page.Waits.Model
    }


type Theme
    = LightMode
    | DarkMode


type Page
    = ScoringPage
    | WaitsPage


init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        i18n =
            I18n.init I18n.En

        ( waitsFlags, darkTheme ) =
            case D.decodeValue flagsDecoder flags of
                Ok res ->
                    res

                Err _ ->
                    ( E.null, "f" )

        -- ( scoring, scoringCmd ) =
        --     Page.Scoring.init
        ( waits, waitsCmd ) =
            Page.Waits.init i18n waitsFlags
    in
    ( { i18n = i18n
      , page = WaitsPage
      , theme =
            if darkTheme == "t" then
                DarkMode

            else
                LightMode

      --   , scoring = scoring
      , waits = waits
      }
    , Cmd.map WaitsMsg waitsCmd
    )


type Msg
    = SetPage Page
    | ToggleTheme
      -- | ScoringMsg Page.Scoring.Msg
    | WaitsMsg Page.Waits.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            ( { model | page = page }, Cmd.none )

        ToggleTheme ->
            let
                ( strTheme, newTheme ) =
                    case model.theme of
                        LightMode ->
                            ( "t", DarkMode )

                        DarkMode ->
                            ( "f", LightMode )
            in
            ( { model | theme = newTheme }, setDarkMode strTheme )

        -- ScoringMsg smsg ->
        --     let
        --         ( scoring, scoringCmd ) =
        --             Page.Scoring.update smsg model.scoring
        --     in
        --     ( { model | scoring = scoring }, Cmd.map ScoringMsg scoringCmd )
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
                    Html.div [] []

                -- Html.map ScoringMsg (Page.Scoring.view model.scoring)
                WaitsPage ->
                    Html.map WaitsMsg (Page.Waits.view model.waits)

        -- isActive targetPage =
        --     if model.page == targetPage then
        --         class "is-active"
        --     else
        --         class ""
    in
    Html.div
        [ class "base-container"
        , themeClass model
        ]
        [ Html.div [ class "container" ]
            [ -- , Html.nav [ class "navbar" ]
              --     [ Html.div [ class "navbar-menu" ]
              --         [ Html.div [ class "navbar-brand" ]
              --             [ Html.a [ class "navbar-item" ] [ Html.text "Mahjong" ] ]
              --         , Html.div [ class "navbar-start" ]
              --             [ Html.a [ class "navbar-item", onClick (SetPage ScoringPage), isActive ScoringPage ] [ Html.text "Scoring" ]
              --             , Html.a [ class "navbar-item", onClick (SetPage WaitsPage), isActive WaitsPage ] [ Html.text "Waits" ]
              --             ]
              --         ]
              --     ]
              Html.h1 [ class "title" ] [ Html.text "Mahjong Waits Trainer" ]
            , Html.a [ onClick ToggleTheme, class "icon-link theme-toggle is-clickable", title (I18n.toggleThemeButtonTitle model.i18n) ] [ UI.icon "icon" (nextThemeIcon model) ]
            , Html.div [ class "main" ] [ content ]
            ]
        , Html.footer [ class "footer" ]
            [ Html.div [ class "has-text-centered" ]
                [ Html.map never <|
                    Html.p [ class "content" ]
                        (I18n.mahjongImageCredits { author = "Martin Persson", href = "https://www.martinpersson.org/" } [] model.i18n)
                , Html.map never <|
                    Html.p [ class "content" ]
                        (I18n.faviconCredits { author = "Freepik - Flaticon", href = "https://www.flaticon.com/free-icons/mahjong" } [] model.i18n)
                , Html.p [ class "mt-2" ] [ Html.a [ class "icon-link", href "https://github.com/djuretic/riichi-mahjong-trainer", target "_blank" ] [ UI.icon "icon" Brands.github ] ]
                ]
            ]
        ]


themeClass : Model -> Html.Attribute msg
themeClass model =
    case model.theme of
        LightMode ->
            class "light-mode"

        DarkMode ->
            class "dark-mode"


nextThemeIcon : Model -> FontAwesome.Icon FontAwesome.WithoutId
nextThemeIcon model =
    case model.theme of
        LightMode ->
            Solid.moon

        DarkMode ->
            Solid.sun


flagsDecoder : D.Decoder ( D.Value, String )
flagsDecoder =
    D.map2 (\a b -> ( a, b ))
        (D.field "waits" D.value)
        (D.field "darkMode" D.string)
