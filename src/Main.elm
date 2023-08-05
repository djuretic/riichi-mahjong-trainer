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
import Page.Efficiency
import Page.Waits
import UI


port setStorageConfig : E.Value -> Cmd msg


port setHtmlClass : String -> Cmd msg


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


type Page
    = ScoringPage
    | WaitsPage
    | EfficiencyPage


type Msg
    = SetPage Page
    | SetTheme Theme
    | SetLanguage I18n.Language
    | WaitsMsg Page.Waits.Msg
    | EfficiencyMsg Page.Efficiency.Msg
    | SetLanguageDropdownOpen Bool
    | ToggleShowConfig
    | TogglePage


defaultConfig : ConfigModel
defaultConfig =
    { language = "en", darkTheme = False }


init : E.Value -> ( Model, Cmd Msg )
init flags =
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
            , page = WaitsPage
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
        , setHtmlClass (themeClassName theme)
        , setStorageConfig (encode model)
        , Cmd.map EfficiencyMsg efficiencyCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            ( { model | page = page }, Cmd.none )

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
            in
            ( newModel, Cmd.batch [ newCmd, setStorageConfig (encode newModel) ] )

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

        ToggleShowConfig ->
            ( { model | showConfig = not model.showConfig }, Cmd.none )

        TogglePage ->
            let
                newPage =
                    case model.page of
                        WaitsPage ->
                            EfficiencyPage

                        EfficiencyPage ->
                            WaitsPage

                        ScoringPage ->
                            EfficiencyPage
            in
            ( { model | page = newPage }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.page == WaitsPage then
        Sub.map WaitsMsg (Page.Waits.subscriptions model.waits)

    else if model.page == EfficiencyPage then
        Sub.map EfficiencyMsg (Page.Efficiency.subscriptions model.efficiency)

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

                EfficiencyPage ->
                    Html.map EfficiencyMsg (Page.Efficiency.view model.efficiency)
    in
    div
        [ class "base-container"
        , onClick (SetLanguageDropdownOpen False)
        ]
        [ navbar model
        , div [ class "container p-2" ]
            [ if model.showConfig then
                settingsUI model

              else
                content
            ]
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


navbar : Model -> Html.Html Msg
navbar model =
    div [ class "navbar container is-flex is-flex-direction-row is-justify-content-space-between p-2" ]
        [ h1 [ class "title is-size-4 mb-0 overflow-ellipsis" ] [ text (I18n.siteTitle model.i18n) ]
        , a [ onClick TogglePage ] [ text "*" ]
        , a
            [ class "icon-link is-clickable p-1 rounded", classList [ ( "has-background-primary", model.showConfig ) ], title (I18n.settingsTitle model.i18n), onClick ToggleShowConfig ]
            [ UI.icon "icon" Solid.gear ]
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
            "light-mode"

        DarkMode ->
            "dark-mode"


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
