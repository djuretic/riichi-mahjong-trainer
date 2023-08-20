port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import FontAwesome.Brands as Brands
import FontAwesome.Solid as Solid
import Html exposing (a, button, div, footer, h1, nav, p, span, text)
import Html.Attributes as HtmlA exposing (class, classList, href, id, target, title)
import Html.Events exposing (onClick, stopPropagationOn)
import I18n
import Json.Decode as D
import Json.Encode as E
import Page.Efficiency
import Page.Waits
import UI
import Url
import Url.Parser


port setStorageConfig : E.Value -> Cmd msg


port setHtmlClass : String -> Cmd msg


main : Program E.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { language : I18n.Language
    , i18n : I18n.I18n
    , navKey : Nav.Key
    , route : Maybe Route
    , lastRoute : Maybe Route
    , navbarBurgerActive : Bool
    , theme : Theme

    -- , scoring : Page.Scoring.Model
    , waits : Page.Waits.Model
    , efficiency : Page.Efficiency.Model
    , languageDropdownOpen : Bool
    , showConfig : Bool
    }


type alias FlagsModel =
    { waits : D.Value
    , efficiency : D.Value
    , config : D.Value
    , browserLanguage : String
    , urlLanguage : String
    }


type alias ConfigModel =
    { language : String
    , darkTheme : Bool
    }


type Theme
    = LightMode
    | DarkMode


type Route
    = ScoringPage
    | WaitsPage
    | EfficiencyPage
    | SettingsPage
    | NotFound


type Msg
    = SetTheme Theme
    | SetLanguage I18n.Language
    | WaitsMsg Page.Waits.Msg
    | EfficiencyMsg Page.Efficiency.Msg
    | SetLanguageDropdownOpen Bool
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
      -- | ToggleShowConfig
    | TogglePage
    | ToggleNavbarBurger


defaultConfig : ConfigModel
defaultConfig =
    { language = "en", darkTheme = False }


init : E.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( flagsModel, configModel ) =
            case D.decodeValue flagsDecoder flags of
                Ok res ->
                    let
                        languageFromQueryString =
                            I18n.languageFromString res.urlLanguage
                                |> Maybe.map I18n.languageToString
                    in
                    case D.decodeValue configDecoder res.config of
                        Ok config ->
                            case languageFromQueryString of
                                Just language ->
                                    ( res, { config | language = language } )

                                Nothing ->
                                    ( res, config )

                        Err _ ->
                            let
                                languageFromBrowser =
                                    I18n.languageFromString res.browserLanguage
                                        |> Maybe.map I18n.languageToString
                                        |> Maybe.withDefault "en"

                                language =
                                    Maybe.withDefault languageFromBrowser languageFromQueryString
                            in
                            -- first visit to the site
                            ( res, { defaultConfig | language = language } )

                Err _ ->
                    ( { waits = E.null, efficiency = E.null, config = E.null, browserLanguage = "", urlLanguage = "" }, defaultConfig )

        lang =
            I18n.languageFromString configModel.language
                |> Maybe.withDefault I18n.En

        i18n =
            I18n.init lang

        ( waits, waitsCmd ) =
            Page.Waits.init i18n flagsModel.waits

        ( efficiency, efficiencyCmd ) =
            Page.Efficiency.init i18n flagsModel.efficiency

        theme =
            if configModel.darkTheme then
                DarkMode

            else
                LightMode

        model =
            { language = lang
            , i18n = i18n
            , navKey = navKey
            , route = Url.Parser.parse routeParser url
            , lastRoute = Nothing
            , navbarBurgerActive = False
            , theme = theme
            , waits = waits
            , efficiency = efficiency
            , languageDropdownOpen = False
            , showConfig = False
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map WaitsMsg waitsCmd
        , Cmd.map EfficiencyMsg efficiencyCmd
        , setHtmlClass (themeClassName theme)
        , setStorageConfig (encode model)
        ]
    )


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map WaitsPage Url.Parser.top
        , Url.Parser.map WaitsPage (Url.Parser.s "waits")
        , Url.Parser.map EfficiencyPage (Url.Parser.s "efficiency")
        , Url.Parser.map SettingsPage (Url.Parser.s "settings")
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTheme theme ->
            let
                newModel =
                    { model | theme = theme }
            in
            ( newModel, Cmd.batch [ setStorageConfig (encode newModel), setHtmlClass (themeClassName newModel.theme) ] )

        SetLanguage lang ->
            let
                newI18n =
                    I18n.init lang

                ( newModel, newCmd ) =
                    update (WaitsMsg (Page.Waits.UpdateI18n newI18n)) { model | language = lang, i18n = newI18n }

                ( newModel2, newCmd2 ) =
                    update (EfficiencyMsg (Page.Efficiency.UpdateI18n newI18n)) { newModel | language = lang, i18n = newI18n }
            in
            ( newModel2, Cmd.batch [ newCmd, newCmd2, setStorageConfig (encode newModel2) ] )

        WaitsMsg wmsg ->
            let
                ( waits, waitsCmd ) =
                    Page.Waits.update wmsg model.waits
            in
            ( { model | waits = waits }, Cmd.map WaitsMsg waitsCmd )

        EfficiencyMsg emsg ->
            let
                ( efficiency, efficiencyCmd ) =
                    Page.Efficiency.update emsg model.efficiency
            in
            ( { model | efficiency = efficiency }, Cmd.map EfficiencyMsg efficiencyCmd )

        SetLanguageDropdownOpen value ->
            ( { model | languageDropdownOpen = value }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            let
                newRoute =
                    Url.Parser.parse routeParser url
            in
            if newRoute == model.route then
                ( model, Cmd.none )

            else
                ( { model | route = newRoute, lastRoute = model.route }, Cmd.none )

        TogglePage ->
            let
                newPage =
                    case model.route of
                        Just WaitsPage ->
                            Just EfficiencyPage

                        Just EfficiencyPage ->
                            Just WaitsPage

                        _ ->
                            model.route
            in
            ( { model | route = newPage }, Cmd.none )

        ToggleNavbarBurger ->
            ( { model | navbarBurgerActive = not model.navbarBurgerActive }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.route == Just WaitsPage then
        Sub.map WaitsMsg (Page.Waits.subscriptions model.waits)

    else if model.route == Just EfficiencyPage then
        Sub.map EfficiencyMsg (Page.Efficiency.subscriptions model.efficiency)

    else
        Sub.none


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.route of
                Just WaitsPage ->
                    Html.map WaitsMsg (Page.Waits.view model.waits)

                Just EfficiencyPage ->
                    Html.map EfficiencyMsg (Page.Efficiency.view model.efficiency)

                Just SettingsPage ->
                    settingsUI model

                _ ->
                    div [] []
    in
    { title = "Mahjong Waits Trainer"
    , body =
        [ div
            [ class "base-container"
            , onClick (SetLanguageDropdownOpen False)
            ]
            [ navbar model
            , div [ class "container p-2" ] [ content ]
            , footer [ class "footer pb-6" ]
                [ div [ class "has-text-centered content" ]
                    [ Html.map never <|
                        div []
                            (I18n.mahjongImageCredits { author = "Martin Persson", href = "https://www.martinpersson.org/" } [] model.i18n)
                    , Html.map never <|
                        div []
                            (I18n.faviconCredits { author = "Freepik - Flaticon", href = "https://www.flaticon.com/free-icons/mahjong" } [] model.i18n)
                    , p [ class "mt-2" ] [ a [ class "icon-link", href "https://github.com/djuretic/riichi-mahjong-trainer", target "_blank" ] [ UI.icon "icon" Brands.github ] ]
                    ]
                ]
            ]
        ]
    }


navbar : Model -> Html.Html Msg
navbar model =
    nav [ class "navbar has-shadow is-fixed-top" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "/" ] [ text "Mahjong Waits Trainer" ]

            -- we add target="_self" as a way to preventDefault() on the click event
            , a
                [ class "navbar-burger"
                , HtmlA.attribute "role" "button"
                , HtmlA.attribute "data-target" "navbarAppMenu"
                , classList [ ( "is-active", model.navbarBurgerActive ) ]
                , onClick ToggleNavbarBurger
                , target "_self"
                ]
                [ span [ HtmlA.attribute "aria-hidden" "true" ] []
                , span [ HtmlA.attribute "aria-hidden" "true" ] []
                , span [ HtmlA.attribute "aria-hidden" "true" ] []
                ]
            ]
        , div [ class "navbar-menu", id "navbarAppMenu", classList [ ( "is-active", model.navbarBurgerActive ) ] ]
            [ div [ class "navbar-start" ]
                [ a [ class "navbar-item", href "/waits", classList [ ( "is-active", model.route == Just WaitsPage ) ] ] [ text (I18n.waitsTitle model.i18n) ]
                , a [ class "navbar-item", href "/efficiency", classList [ ( "is-active", model.route == Just EfficiencyPage ) ] ] [ text (I18n.efficiencyTitle model.i18n) ]
                ]
            , div [ class "navbar-end" ]
                [ a [ class "navbar-item", href "/settings", classList [ ( "is-active", model.route == Just SettingsPage ) ] ]
                    [ span [ class "icon-text" ]
                        [ UI.icon "icon" Solid.gear
                        , span [] [ text (I18n.settingsTitle model.i18n) ]
                        ]
                    ]
                ]
            ]
        ]


settingsUI : Model -> Html.Html Msg
settingsUI model =
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
    div [ class "box mb-5" ]
        [ Html.h3 [ class "title is-5" ] [ text (I18n.settingsTitle model.i18n) ]
        , UI.label (I18n.languageSelectorTitle model.i18n) langSelector
        , UI.label (I18n.themeSelectorTitle model.i18n) themeSelector
        ]


themeClassName : Theme -> String
themeClassName theme =
    case theme of
        LightMode ->
            "light-mode has-navbar-fixed-top"

        DarkMode ->
            "dark-mode has-navbar-fixed-top"


flagsDecoder : D.Decoder FlagsModel
flagsDecoder =
    D.map5 FlagsModel
        (D.field "waits" D.value)
        (D.field "efficiency" D.value)
        (D.field "config" D.value)
        (D.field "browserLanguage" D.string)
        (D.field "urlLanguage" D.string)


configDecoder : D.Decoder ConfigModel
configDecoder =
    D.map2 ConfigModel
        (D.field "lang" D.string)
        (D.field "darkMode" D.bool)


languageName : I18n.Language -> String
languageName lang =
    case lang of
        I18n.En ->
            "English"

        I18n.Eo ->
            "Esperanto"

        I18n.Es ->
            "Español"


encode : Model -> E.Value
encode model =
    E.object
        [ ( "lang", E.string (I18n.languageToString model.language) )
        , ( "darkMode", E.bool (model.theme == DarkMode) )
        ]
