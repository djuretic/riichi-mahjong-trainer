module I18n exposing (I18n, Language(..), animationTab, confirmTilesButton, correctAnswer, currentLanguage, eastWindDescription, efficiencyTitle, faviconCredits, greenDragonDescription, groupsContentPlaceholder, init, languageFromString, languageSelectorTitle, languageToString, languages, linksTab, load, mahjongImageCredits, manTileDescription, minWaitsSelectorTitle, newHandButton, northWindDescription, numTilesSelectorTitle, numberedTilesSelector, numberedTilesSelectorNo, numberedTilesSelectorYes, pinTileDescription, redDragonDescription, remainingTime, selectWaitTilesText, settingsTitle, showWaitTilesText, siteTitle, souTileDescription, southWindDescription, suitSelectorTitle, suitSelectorTitleHonor, suitSelectorTitleMan, suitSelectorTitlePin, suitSelectorTitleRandom, suitSelectorTitleSou, suitSelectorTitleWithNumber, tableTab, themeSelectorTitle, themeSelectorTitleDark, themeSelectorTitleLight, tileNumber1, tileNumber2, tileNumber3, tileNumber4, tileNumber5, tileNumber6, tileNumber7, tileNumber8, tileNumber9, timerOff, timerSelector, trainWaitsMode, trainWaitsModeOne, trainWaitsModeTwo, waitsTitle, westWindDescription, whiteDragonDescription, wrongAnswer)

{-| This file was generated by travelm-agency version 3.4.1.

If you have any issues with the generated code, do not hesitate to open an issue here: <https://github.com/andreasewering/travelm-agency/issues>

-}

import Html
import Html.Attributes
import List
import Maybe
import String


{-| Initialize an i18n instance based on a language
-}
init : Language -> I18n
init =
    I18n


{-| Get the currently active language.
-}
currentLanguage : I18n -> Language
currentLanguage (I18n lang) =
    lang


{-| Switch to another i18n instance based on a language
-}
load : Language -> I18n -> I18n
load lang _ =
    init lang


type I18n
    = I18n Language


animationTab : I18n -> String
animationTab (I18n lang) =
    case lang of
        En ->
            "Animation"

        Eo ->
            "Movbildo"

        Es ->
            "Animación"


confirmTilesButton : I18n -> String
confirmTilesButton (I18n lang) =
    case lang of
        En ->
            "Confirm"

        Eo ->
            "Konfirmi"

        Es ->
            "Confirmar"


correctAnswer : I18n -> String
correctAnswer (I18n lang) =
    case lang of
        En ->
            "Correct!"

        Eo ->
            "Ĝuste!"

        Es ->
            "¡Correcto!"


eastWindDescription : I18n -> String
eastWindDescription (I18n lang) =
    case lang of
        En ->
            "east wind"

        Eo ->
            "orienta vento"

        Es ->
            "viento este"


efficiencyTitle : I18n -> String
efficiencyTitle (I18n lang) =
    case lang of
        En ->
            "Efficiency"

        Eo ->
            "Efikeco"

        Es ->
            "Eficiencia"


faviconCredits : { author : String, href : String } -> List (Html.Attribute msg) -> I18n -> List (Html.Html msg)
faviconCredits data aAttrs (I18n lang) =
    case lang of
        En ->
            [ Html.text "Favicon by "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]

        Eo ->
            [ Html.text "Retpaĝsimbolo el "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]

        Es ->
            [ Html.text "Favicon por "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]


greenDragonDescription : I18n -> String
greenDragonDescription (I18n lang) =
    case lang of
        En ->
            "green dragon"

        Eo ->
            "verda drako"

        Es ->
            "dragón verde"


groupsContentPlaceholder : I18n -> String
groupsContentPlaceholder (I18n lang) =
    case lang of
        En ->
            "Select waits to view possible groups"

        Eo ->
            "Elektu la atendojn por vidi la eblajn grupojn"

        Es ->
            "Seleciona las esperas para ver los grupos posibles"


languageSelectorTitle : I18n -> String
languageSelectorTitle (I18n lang) =
    case lang of
        En ->
            "Language"

        Eo ->
            "Lingvo"

        Es ->
            "Idioma"


linksTab : I18n -> String
linksTab (I18n lang) =
    case lang of
        En ->
            "Links"

        Eo ->
            "Ligiloj"

        Es ->
            "Enlaces"


mahjongImageCredits : { author : String, href : String } -> List (Html.Attribute msg) -> I18n -> List (Html.Html msg)
mahjongImageCredits data aAttrs (I18n lang) =
    case lang of
        En ->
            [ Html.text "Mahjong tile images by "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]

        Eo ->
            [ Html.text "Bildoj de maĝangaj pecoj el "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]

        Es ->
            [ Html.text "Imágenes de fichas de mahjong por "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]


manTileDescription : String -> I18n -> String
manTileDescription number (I18n lang) =
    case lang of
        En ->
            number ++ " of characters"

        Eo ->
            number ++ " de signoj"

        Es ->
            number ++ " de caracteres"


minWaitsSelectorTitle : I18n -> String
minWaitsSelectorTitle (I18n lang) =
    case lang of
        En ->
            "Min. number of waits"

        Eo ->
            "Min. nombro da atendoj"

        Es ->
            "Cantidad mín. de esperas"


newHandButton : I18n -> String
newHandButton (I18n lang) =
    case lang of
        En ->
            "New hand"

        Eo ->
            "Nova mano"

        Es ->
            "Nueva mano"


northWindDescription : I18n -> String
northWindDescription (I18n lang) =
    case lang of
        En ->
            "north wind"

        Eo ->
            "norda vento"

        Es ->
            "viento norte"


numTilesSelectorTitle : I18n -> String
numTilesSelectorTitle (I18n lang) =
    case lang of
        En ->
            "Number of tiles"

        Eo ->
            "Nombro da pecoj"

        Es ->
            "Cantidad de fichas"


numberedTilesSelector : I18n -> String
numberedTilesSelector (I18n lang) =
    case lang of
        En ->
            "Numbered tiles"

        Eo ->
            "Numeritaj pecoj"

        Es ->
            "Fichas numeradas"


numberedTilesSelectorNo : I18n -> String
numberedTilesSelectorNo (I18n lang) =
    case lang of
        En ->
            "No"

        Eo ->
            "Ne"

        Es ->
            "No"


numberedTilesSelectorYes : I18n -> String
numberedTilesSelectorYes (I18n lang) =
    case lang of
        En ->
            "Yes"

        Eo ->
            "Jes"

        Es ->
            "Sí"


pinTileDescription : String -> I18n -> String
pinTileDescription number (I18n lang) =
    case lang of
        En ->
            number ++ " of circles"

        Eo ->
            number ++ " de cirkloj"

        Es ->
            number ++ " de círculos"


redDragonDescription : I18n -> String
redDragonDescription (I18n lang) =
    case lang of
        En ->
            "red dragon"

        Eo ->
            "ruĝa drako"

        Es ->
            "dragón rojo"


remainingTime : I18n -> String
remainingTime (I18n lang) =
    case lang of
        En ->
            "Remaining Time"

        Eo ->
            "Restanta tempo"

        Es ->
            "Tiempo restante"


selectWaitTilesText : I18n -> String
selectWaitTilesText (I18n lang) =
    case lang of
        En ->
            "Select wait tiles:"

        Eo ->
            "Elektu la atendojn:"

        Es ->
            "Selecciona las esperas:"


settingsTitle : I18n -> String
settingsTitle (I18n lang) =
    case lang of
        En ->
            "Settings"

        Eo ->
            "Agordoj"

        Es ->
            "Configuración"


showWaitTilesText : I18n -> String
showWaitTilesText (I18n lang) =
    case lang of
        En ->
            "Wait tiles:"

        Eo ->
            "Atendoj:"

        Es ->
            "Esperas:"


siteTitle : I18n -> String
siteTitle (I18n lang) =
    case lang of
        En ->
            "Mahjong Waits Trainer"

        Eo ->
            "Ekzercilo de atendoj de maĝango"

        Es ->
            "Práctica de esperas de mahjong"


souTileDescription : String -> I18n -> String
souTileDescription number (I18n lang) =
    case lang of
        En ->
            number ++ " of bamboo"

        Eo ->
            number ++ " de bambuoj"

        Es ->
            number ++ " de bambú"


southWindDescription : I18n -> String
southWindDescription (I18n lang) =
    case lang of
        En ->
            "south wind"

        Eo ->
            "suda vento"

        Es ->
            "viento sur"


suitSelectorTitle : I18n -> String
suitSelectorTitle (I18n lang) =
    case lang of
        En ->
            "Suit"

        Eo ->
            "Emblemoj"

        Es ->
            "Palo"


suitSelectorTitleHonor : I18n -> String
suitSelectorTitleHonor (I18n lang) =
    case lang of
        En ->
            "Honors"

        Eo ->
            "Honoroj"

        Es ->
            "Honores"


suitSelectorTitleMan : I18n -> String
suitSelectorTitleMan (I18n lang) =
    case lang of
        En ->
            "Characters"

        Eo ->
            "Signoj"

        Es ->
            "Caracteres"


suitSelectorTitlePin : I18n -> String
suitSelectorTitlePin (I18n lang) =
    case lang of
        En ->
            "Circles"

        Eo ->
            "Cirkloj"

        Es ->
            "Círculos"


suitSelectorTitleRandom : I18n -> String
suitSelectorTitleRandom (I18n lang) =
    case lang of
        En ->
            "Random"

        Eo ->
            "Hazardaj"

        Es ->
            "Al azar"


suitSelectorTitleSou : I18n -> String
suitSelectorTitleSou (I18n lang) =
    case lang of
        En ->
            "Bamboos "

        Eo ->
            "Bambuoj"

        Es ->
            "Bambúes"


suitSelectorTitleWithNumber : String -> I18n -> String
suitSelectorTitleWithNumber number (I18n lang) =
    case lang of
        En ->
            "Suit " ++ number

        Eo ->
            "Emblemoj " ++ number

        Es ->
            "Palo " ++ number


tableTab : I18n -> String
tableTab (I18n lang) =
    case lang of
        En ->
            "Table"

        Eo ->
            "Tabelo"

        Es ->
            "Tabla"


themeSelectorTitle : I18n -> String
themeSelectorTitle (I18n lang) =
    case lang of
        En ->
            "Theme"

        Eo ->
            "Etoso"

        Es ->
            "Tema"


themeSelectorTitleDark : I18n -> String
themeSelectorTitleDark (I18n lang) =
    case lang of
        En ->
            "Dark"

        Eo ->
            "Malhela"

        Es ->
            "Oscuro"


themeSelectorTitleLight : I18n -> String
themeSelectorTitleLight (I18n lang) =
    case lang of
        En ->
            "Light"

        Eo ->
            "Hela"

        Es ->
            "Claro"


tileNumber1 : I18n -> String
tileNumber1 (I18n lang) =
    case lang of
        En ->
            "one"

        Eo ->
            "unu"

        Es ->
            "uno"


tileNumber2 : I18n -> String
tileNumber2 (I18n lang) =
    case lang of
        En ->
            "two"

        Eo ->
            "du"

        Es ->
            "dos"


tileNumber3 : I18n -> String
tileNumber3 (I18n lang) =
    case lang of
        En ->
            "three"

        Eo ->
            "tri"

        Es ->
            "tres"


tileNumber4 : I18n -> String
tileNumber4 (I18n lang) =
    case lang of
        En ->
            "four"

        Eo ->
            "kvar"

        Es ->
            "cuatro"


tileNumber5 : I18n -> String
tileNumber5 (I18n lang) =
    case lang of
        En ->
            "five"

        Eo ->
            "kvin"

        Es ->
            "cinco"


tileNumber6 : I18n -> String
tileNumber6 (I18n lang) =
    case lang of
        En ->
            "six"

        Eo ->
            "ses"

        Es ->
            "seis"


tileNumber7 : I18n -> String
tileNumber7 (I18n lang) =
    case lang of
        En ->
            "seven"

        Eo ->
            "sep"

        Es ->
            "siete"


tileNumber8 : I18n -> String
tileNumber8 (I18n lang) =
    case lang of
        En ->
            "eight"

        Eo ->
            "ok"

        Es ->
            "ocho"


tileNumber9 : I18n -> String
tileNumber9 (I18n lang) =
    case lang of
        En ->
            "nine"

        Eo ->
            "naŭ"

        Es ->
            "nueve"


timerOff : I18n -> String
timerOff (I18n lang) =
    case lang of
        En ->
            "Off"

        Eo ->
            "Malŝaltita"

        Es ->
            "Apagado"


timerSelector : I18n -> String
timerSelector (I18n lang) =
    case lang of
        En ->
            "Timer"

        Eo ->
            "Tempmezurilo"

        Es ->
            "Temporizador"


trainWaitsMode : I18n -> String
trainWaitsMode (I18n lang) =
    case lang of
        En ->
            "Mode"

        Eo ->
            "Reĝimo"

        Es ->
            "Modo"


trainWaitsModeOne : I18n -> String
trainWaitsModeOne (I18n lang) =
    case lang of
        En ->
            "One suit"

        Eo ->
            "Unu emblemo"

        Es ->
            "Un palo"


trainWaitsModeTwo : I18n -> String
trainWaitsModeTwo (I18n lang) =
    case lang of
        En ->
            "Two suits"

        Eo ->
            "Du emblemoj"

        Es ->
            "Dos palos"


waitsTitle : I18n -> String
waitsTitle (I18n lang) =
    case lang of
        En ->
            "Waits"

        Eo ->
            "Atendoj"

        Es ->
            "Esperas"


westWindDescription : I18n -> String
westWindDescription (I18n lang) =
    case lang of
        En ->
            "west wind"

        Eo ->
            "okcidenta vento"

        Es ->
            "viento oeste"


whiteDragonDescription : I18n -> String
whiteDragonDescription (I18n lang) =
    case lang of
        En ->
            "white dragon"

        Eo ->
            "blanka drako"

        Es ->
            "dragón blanco"


wrongAnswer : I18n -> String
wrongAnswer (I18n lang) =
    case lang of
        En ->
            "Wrong"

        Eo ->
            "Malĝuste"

        Es ->
            "Incorrecto"


{-| Enumeration of the supported languages
-}
type Language
    = En
    | Eo
    | Es


{-| A list containing all `Language`s. The list is sorted alphabetically.
-}
languages : List Language
languages =
    [ En, Eo, Es ]


{-| Convert a `Language` to its `String` representation.
-}
languageToString : Language -> String
languageToString lang =
    case lang of
        En ->
            "en"

        Eo ->
            "eo"

        Es ->
            "es"


{-| Maybe parse a `Language` from a `String`.
This will map languages based on the prefix i.e. 'en-US' and 'en' will both map to 'En' unless you provided a 'en-US' translation file.
-}
languageFromString : String -> Maybe Language
languageFromString lang =
    let
        helper langs =
            case langs of
                [] ->
                    Maybe.Nothing

                l :: ls ->
                    if String.startsWith (languageToString l) lang then
                        Maybe.Just l

                    else
                        helper ls
    in
    helper (List.reverse languages)
