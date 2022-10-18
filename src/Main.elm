port module Main exposing (main)

import Browser
import FontAwesome.Brands as Brands
import FontAwesome.Solid as Solid
import Html exposing (a, button, div, footer, h1, p, span, text)
import Html.Attributes as HtmlA exposing (class, classList, href, id, target, title)
import Html.Events exposing (onClick, stopPropagationOn)
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
    { language : I18n.Language
    , i18n : I18n.I18n
    , page : Page
    , theme : Theme

    -- , scoring : Page.Scoring.Model
    , waits : Page.Waits.Model
    , languageDropdownOpen : Bool
    }


type Theme
    = LightMode
    | DarkMode


type Page
    = ScoringPage
    | WaitsPage


type Msg
    = SetPage Page
    | SetTheme Theme
    | SetLanguage I18n.Language
      -- | ScoringMsg Page.Scoring.Msg
    | WaitsMsg Page.Waits.Msg
    | SetLanguageDropdownOpen Bool


init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        lang =
            I18n.Es

        i18n =
            I18n.init lang

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
    ( { language = lang
      , i18n = i18n
      , page = WaitsPage
      , theme =
            if darkTheme == "t" then
                DarkMode

            else
                LightMode

      --   , scoring = scoring
      , waits = waits
      , languageDropdownOpen = False
      }
    , Cmd.map WaitsMsg waitsCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            ( { model | page = page }, Cmd.none )

        SetTheme theme ->
            let
                strTheme =
                    case model.theme of
                        LightMode ->
                            "f"

                        DarkMode ->
                            "t"
            in
            ( { model | theme = theme }, setDarkMode strTheme )

        SetLanguage lang ->
            let
                newI18n =
                    I18n.init lang
            in
            update (WaitsMsg (Page.Waits.UpdateI18n newI18n)) { model | language = lang, i18n = newI18n }

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

        SetLanguageDropdownOpen value ->
            ( { model | languageDropdownOpen = value }, Cmd.none )


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
                    div [] []

                WaitsPage ->
                    Html.map WaitsMsg (Page.Waits.view model.waits)
    in
    div
        [ class "base-container"
        , themeClass model
        , onClick (SetLanguageDropdownOpen False)
        ]
        [ div [ class "container" ]
            [ h1 [ class "title is-size-4" ] [ text (I18n.siteTitle model.i18n) ]
            , a [ class "icon-link theme-toggle is-clickable", title (I18n.toggleThemeButtonTitle model.i18n) ] [ UI.icon "icon" Solid.gear ]
            , renderSettings model
            , div [ class "main" ] [ content ]
            ]
        , footer [ class "footer" ]
            [ div [ class "has-text-centered" ]
                [ Html.map never <|
                    p [ class "content" ]
                        (I18n.mahjongImageCredits { author = "Martin Persson", href = "https://www.martinpersson.org/" } [] model.i18n)
                , Html.map never <|
                    p [ class "content" ]
                        (I18n.faviconCredits { author = "Freepik - Flaticon", href = "https://www.flaticon.com/free-icons/mahjong" } [] model.i18n)
                , p [ class "mt-2" ] [ a [ class "icon-link", href "https://github.com/djuretic/riichi-mahjong-trainer", target "_blank" ] [ UI.icon "icon" Brands.github ] ]
                ]
            ]
        ]


renderSettings : Model -> Html.Html Msg
renderSettings model =
    let
        langSelector =
            div
                [ classList
                    [ ( "dropdown", True )
                    , ( "is-active", model.languageDropdownOpen )
                    ]
                , stopPropagationOn "click" (D.succeed ( SetLanguageDropdownOpen (not model.languageDropdownOpen), True ))
                ]
                [ div [ class "dropdown-trigger" ]
                    [ button [ class "button", HtmlA.attribute "aria-haspopup" "true", HtmlA.attribute "aria-controls" "dropdown-menu" ]
                        [ span [] [ text (languageName model.language) ]
                        , UI.icon "icon is-small" Solid.angleDown
                        ]
                    ]
                , div [ id "dropdown-menu", class "dropdown-menu", HtmlA.attribute "role" "menu" ]
                    [ div [ class "dropdown-content" ]
                        (List.map
                            (\lang ->
                                a
                                    [ href "#", class "dropdown-item", classList [ ( "is-active", model.language == lang ) ], onClick (SetLanguage lang) ]
                                    [ text (languageName lang) ]
                            )
                            I18n.languages
                        )
                    ]
                ]

        themeButton txt theme =
            button
                [ classList
                    [ ( "button", True )
                    , ( "is-primary", model.theme == theme )
                    , ( "is-selected", model.theme == theme )
                    ]
                , onClick (SetTheme theme)
                ]
                [ text txt ]

        themeSelector =
            div [ class "buttons has-addons" ]
                [ themeButton (I18n.themeSelectorTitleLight model.i18n) LightMode
                , themeButton (I18n.themeSelectorTitleDark model.i18n) DarkMode
                ]
    in
    div [ class "box" ]
        [ UI.label (I18n.languageSelectorTitle model.i18n) langSelector
        , UI.label (I18n.themeSelectorTitle model.i18n) themeSelector
        ]


themeClass : Model -> Html.Attribute msg
themeClass model =
    case model.theme of
        LightMode ->
            class "light-mode"

        DarkMode ->
            class "dark-mode"


flagsDecoder : D.Decoder ( D.Value, String )
flagsDecoder =
    D.map2 (\a b -> ( a, b ))
        (D.field "waits" D.value)
        (D.field "darkMode" D.string)


languageName : I18n.Language -> String
languageName lang =
    case lang of
        I18n.En ->
            "English"

        I18n.Es ->
            "Español"
