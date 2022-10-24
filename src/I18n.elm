module I18n exposing (I18n, Language(..), animationTab, confirmTilesButton, correctAnswer, eastWindDescription, faviconCredits, greenDragonDescription, groupsContentPlaceholder, init, languageFromString, languageSelectorTitle, languageToString, languages, load, mahjongImageCredits, manTileDescription, minWaitsSelectorTitle, newHandButton, northWindDescription, numTilesSelectorTitle, numberedTilesSelector, numberedTilesSelectorNo, numberedTilesSelectorYes, pinTileDescription, redDragonDescription, selectWaitTilesText, settingsTitle, showWaitTilesText, siteTitle, souTileDescription, southWindDescription, suitSelectorTitle, suitSelectorTitleMan, suitSelectorTitlePin, suitSelectorTitleRandom, suitSelectorTitleSou, tableTab, themeSelectorTitle, themeSelectorTitleDark, themeSelectorTitleLight, tileNumber1, tileNumber2, tileNumber3, tileNumber4, tileNumber5, tileNumber6, tileNumber7, tileNumber8, tileNumber9, westWindDescription, whiteDragonDescription, wrongAnswer)

{-| This file was generated by travelm-agency version 3.2.0.

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
init lang =
    case lang of
        En ->
            en

        Eo ->
            eo

        Es ->
            es


{-| Switch to another i18n instance based on a language
-}
load : Language -> I18n -> I18n
load lang _ =
    init lang


type alias I18n =
    { animationTab_ : String
    , confirmTilesButton_ : String
    , correctAnswer_ : String
    , eastWindDescription_ : String
    , faviconCredits_ : { author : String, href : String } -> List (Html.Attribute Never) -> List (Html.Html Never)
    , greenDragonDescription_ : String
    , groupsContentPlaceholder_ : String
    , languageSelectorTitle_ : String
    , mahjongImageCredits_ : { author : String, href : String } -> List (Html.Attribute Never) -> List (Html.Html Never)
    , manTileDescription_ : String -> String
    , minWaitsSelectorTitle_ : String
    , newHandButton_ : String
    , northWindDescription_ : String
    , numTilesSelectorTitle_ : String
    , numberedTilesSelector_ : String
    , numberedTilesSelectorNo_ : String
    , numberedTilesSelectorYes_ : String
    , pinTileDescription_ : String -> String
    , redDragonDescription_ : String
    , selectWaitTilesText_ : String
    , settingsTitle_ : String
    , showWaitTilesText_ : String
    , siteTitle_ : String
    , souTileDescription_ : String -> String
    , southWindDescription_ : String
    , suitSelectorTitle_ : String
    , suitSelectorTitleMan_ : String
    , suitSelectorTitlePin_ : String
    , suitSelectorTitleRandom_ : String
    , suitSelectorTitleSou_ : String
    , tableTab_ : String
    , themeSelectorTitle_ : String
    , themeSelectorTitleDark_ : String
    , themeSelectorTitleLight_ : String
    , tileNumber1_ : String
    , tileNumber2_ : String
    , tileNumber3_ : String
    , tileNumber4_ : String
    , tileNumber5_ : String
    , tileNumber6_ : String
    , tileNumber7_ : String
    , tileNumber8_ : String
    , tileNumber9_ : String
    , westWindDescription_ : String
    , whiteDragonDescription_ : String
    , wrongAnswer_ : String
    }


animationTab : I18n -> String
animationTab i18n =
    i18n.animationTab_


confirmTilesButton : I18n -> String
confirmTilesButton i18n =
    i18n.confirmTilesButton_


correctAnswer : I18n -> String
correctAnswer i18n =
    i18n.correctAnswer_


eastWindDescription : I18n -> String
eastWindDescription i18n =
    i18n.eastWindDescription_


faviconCredits : { author : String, href : String } -> List (Html.Attribute Never) -> I18n -> List (Html.Html Never)
faviconCredits data htmlAttrs i18n =
    i18n.faviconCredits_ data htmlAttrs


greenDragonDescription : I18n -> String
greenDragonDescription i18n =
    i18n.greenDragonDescription_


groupsContentPlaceholder : I18n -> String
groupsContentPlaceholder i18n =
    i18n.groupsContentPlaceholder_


languageSelectorTitle : I18n -> String
languageSelectorTitle i18n =
    i18n.languageSelectorTitle_


mahjongImageCredits : { author : String, href : String } -> List (Html.Attribute Never) -> I18n -> List (Html.Html Never)
mahjongImageCredits data htmlAttrs i18n =
    i18n.mahjongImageCredits_ data htmlAttrs


manTileDescription : String -> I18n -> String
manTileDescription data i18n =
    i18n.manTileDescription_ data


minWaitsSelectorTitle : I18n -> String
minWaitsSelectorTitle i18n =
    i18n.minWaitsSelectorTitle_


newHandButton : I18n -> String
newHandButton i18n =
    i18n.newHandButton_


northWindDescription : I18n -> String
northWindDescription i18n =
    i18n.northWindDescription_


numTilesSelectorTitle : I18n -> String
numTilesSelectorTitle i18n =
    i18n.numTilesSelectorTitle_


numberedTilesSelector : I18n -> String
numberedTilesSelector i18n =
    i18n.numberedTilesSelector_


numberedTilesSelectorNo : I18n -> String
numberedTilesSelectorNo i18n =
    i18n.numberedTilesSelectorNo_


numberedTilesSelectorYes : I18n -> String
numberedTilesSelectorYes i18n =
    i18n.numberedTilesSelectorYes_


pinTileDescription : String -> I18n -> String
pinTileDescription data i18n =
    i18n.pinTileDescription_ data


redDragonDescription : I18n -> String
redDragonDescription i18n =
    i18n.redDragonDescription_


selectWaitTilesText : I18n -> String
selectWaitTilesText i18n =
    i18n.selectWaitTilesText_


settingsTitle : I18n -> String
settingsTitle i18n =
    i18n.settingsTitle_


showWaitTilesText : I18n -> String
showWaitTilesText i18n =
    i18n.showWaitTilesText_


siteTitle : I18n -> String
siteTitle i18n =
    i18n.siteTitle_


souTileDescription : String -> I18n -> String
souTileDescription data i18n =
    i18n.souTileDescription_ data


southWindDescription : I18n -> String
southWindDescription i18n =
    i18n.southWindDescription_


suitSelectorTitle : I18n -> String
suitSelectorTitle i18n =
    i18n.suitSelectorTitle_


suitSelectorTitleMan : I18n -> String
suitSelectorTitleMan i18n =
    i18n.suitSelectorTitleMan_


suitSelectorTitlePin : I18n -> String
suitSelectorTitlePin i18n =
    i18n.suitSelectorTitlePin_


suitSelectorTitleRandom : I18n -> String
suitSelectorTitleRandom i18n =
    i18n.suitSelectorTitleRandom_


suitSelectorTitleSou : I18n -> String
suitSelectorTitleSou i18n =
    i18n.suitSelectorTitleSou_


tableTab : I18n -> String
tableTab i18n =
    i18n.tableTab_


themeSelectorTitle : I18n -> String
themeSelectorTitle i18n =
    i18n.themeSelectorTitle_


themeSelectorTitleDark : I18n -> String
themeSelectorTitleDark i18n =
    i18n.themeSelectorTitleDark_


themeSelectorTitleLight : I18n -> String
themeSelectorTitleLight i18n =
    i18n.themeSelectorTitleLight_


tileNumber1 : I18n -> String
tileNumber1 i18n =
    i18n.tileNumber1_


tileNumber2 : I18n -> String
tileNumber2 i18n =
    i18n.tileNumber2_


tileNumber3 : I18n -> String
tileNumber3 i18n =
    i18n.tileNumber3_


tileNumber4 : I18n -> String
tileNumber4 i18n =
    i18n.tileNumber4_


tileNumber5 : I18n -> String
tileNumber5 i18n =
    i18n.tileNumber5_


tileNumber6 : I18n -> String
tileNumber6 i18n =
    i18n.tileNumber6_


tileNumber7 : I18n -> String
tileNumber7 i18n =
    i18n.tileNumber7_


tileNumber8 : I18n -> String
tileNumber8 i18n =
    i18n.tileNumber8_


tileNumber9 : I18n -> String
tileNumber9 i18n =
    i18n.tileNumber9_


westWindDescription : I18n -> String
westWindDescription i18n =
    i18n.westWindDescription_


whiteDragonDescription : I18n -> String
whiteDragonDescription i18n =
    i18n.whiteDragonDescription_


wrongAnswer : I18n -> String
wrongAnswer i18n =
    i18n.wrongAnswer_


{-| `I18n` instance containing all values for the language En
-}
en : I18n
en =
    { animationTab_ = "Animation"
    , confirmTilesButton_ = "Confirm"
    , correctAnswer_ = "Correct!"
    , eastWindDescription_ = "east wind"
    , faviconCredits_ =
        \data aAttrs ->
            [ Html.text "Favicon by "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]
    , greenDragonDescription_ = "green dragon"
    , groupsContentPlaceholder_ = "Select waits to view possible groups"
    , languageSelectorTitle_ = "Language"
    , mahjongImageCredits_ =
        \data aAttrs ->
            [ Html.text "Mahjong tile images by "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]
    , manTileDescription_ = \number -> number ++ " of characters"
    , minWaitsSelectorTitle_ = "Min. number of waits"
    , newHandButton_ = "New hand"
    , northWindDescription_ = "north wind"
    , numTilesSelectorTitle_ = "Number of tiles"
    , numberedTilesSelector_ = "Numbered tiles"
    , numberedTilesSelectorNo_ = "No"
    , numberedTilesSelectorYes_ = "Yes"
    , pinTileDescription_ = \number -> number ++ " of circles"
    , redDragonDescription_ = "red dragon"
    , selectWaitTilesText_ = "Select wait tiles:"
    , settingsTitle_ = "Settings"
    , showWaitTilesText_ = "Wait tiles:"
    , siteTitle_ = "Mahjong Waits Trainer"
    , souTileDescription_ = \number -> number ++ " of bamboo"
    , southWindDescription_ = "south wind"
    , suitSelectorTitle_ = "Suit"
    , suitSelectorTitleMan_ = "Characters"
    , suitSelectorTitlePin_ = "Circles"
    , suitSelectorTitleRandom_ = "Random"
    , suitSelectorTitleSou_ = "Bamboos "
    , tableTab_ = "Table"
    , themeSelectorTitle_ = "Theme"
    , themeSelectorTitleDark_ = "Dark"
    , themeSelectorTitleLight_ = "Light"
    , tileNumber1_ = "one"
    , tileNumber2_ = "two"
    , tileNumber3_ = "three"
    , tileNumber4_ = "four"
    , tileNumber5_ = "five"
    , tileNumber6_ = "six"
    , tileNumber7_ = "seven"
    , tileNumber8_ = "eight"
    , tileNumber9_ = "nine"
    , westWindDescription_ = "west wind"
    , whiteDragonDescription_ = "white dragon"
    , wrongAnswer_ = "Wrong"
    }


{-| `I18n` instance containing all values for the language Eo
-}
eo : I18n
eo =
    { animationTab_ = "Movbildo"
    , confirmTilesButton_ = "Konfirmi"
    , correctAnswer_ = "Ĝuste!"
    , eastWindDescription_ = "orienta vento"
    , faviconCredits_ =
        \data aAttrs ->
            [ Html.text "Retpaĝsimbolo el "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]
    , greenDragonDescription_ = "verda drako"
    , groupsContentPlaceholder_ = "Elektu la atendojn por vidi la eblajn grupojn"
    , languageSelectorTitle_ = "Lingvo"
    , mahjongImageCredits_ =
        \data aAttrs ->
            [ Html.text "Bildoj de maĝangaj pecoj el "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]
    , manTileDescription_ = \number -> number ++ " de signoj"
    , minWaitsSelectorTitle_ = "Min. nombro da atendoj"
    , newHandButton_ = "Nova mano"
    , northWindDescription_ = "norda vento"
    , numTilesSelectorTitle_ = "Nombro da pecoj"
    , numberedTilesSelector_ = "Numeritaj pecoj"
    , numberedTilesSelectorNo_ = "Ne"
    , numberedTilesSelectorYes_ = "Jes"
    , pinTileDescription_ = \number -> number ++ " de cirkloj"
    , redDragonDescription_ = "ruĝa drako"
    , selectWaitTilesText_ = "Elektu la atendojn:"
    , settingsTitle_ = "Agordoj"
    , showWaitTilesText_ = "Atendoj:"
    , siteTitle_ = "Trejnisto de atendoj de maĝango"
    , souTileDescription_ = \number -> number ++ " de bambuoj"
    , southWindDescription_ = "suda vento"
    , suitSelectorTitle_ = "Emblemoj"
    , suitSelectorTitleMan_ = "Signoj"
    , suitSelectorTitlePin_ = "Cirkloj"
    , suitSelectorTitleRandom_ = "Hazardaj"
    , suitSelectorTitleSou_ = "Bambuoj"
    , tableTab_ = "Tabelo"
    , themeSelectorTitle_ = "Reĝimo"
    , themeSelectorTitleDark_ = "Malhela"
    , themeSelectorTitleLight_ = "Hela"
    , tileNumber1_ = "unu"
    , tileNumber2_ = "du"
    , tileNumber3_ = "tri"
    , tileNumber4_ = "kvar"
    , tileNumber5_ = "kvin"
    , tileNumber6_ = "ses"
    , tileNumber7_ = "sep"
    , tileNumber8_ = "ok"
    , tileNumber9_ = "naŭ"
    , westWindDescription_ = "okcidenta vento"
    , whiteDragonDescription_ = "blanka drako"
    , wrongAnswer_ = "Malĝuste"
    }


{-| `I18n` instance containing all values for the language Es
-}
es : I18n
es =
    { animationTab_ = "Animación"
    , confirmTilesButton_ = "Confirmar"
    , correctAnswer_ = "¡Correcto!"
    , eastWindDescription_ = "viento este"
    , faviconCredits_ =
        \data aAttrs ->
            [ Html.text "Favicon por "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]
    , greenDragonDescription_ = "dragón verde"
    , groupsContentPlaceholder_ = "Seleciona las esperas para ver los grupos posibles"
    , languageSelectorTitle_ = "Idioma"
    , mahjongImageCredits_ =
        \data aAttrs ->
            [ Html.text "Imágenes de fichas de mahjong por "
            , Html.node
                "a"
                ([ Html.Attributes.attribute "href" data.href, Html.Attributes.attribute "target" "_blank" ] ++ aAttrs)
                [ Html.text data.author ]
            ]
    , manTileDescription_ = \number -> number ++ " de caracteres"
    , minWaitsSelectorTitle_ = "Cantidad mín. de esperas"
    , newHandButton_ = "Nueva mano"
    , northWindDescription_ = "viento norte"
    , numTilesSelectorTitle_ = "Cantidad de fichas"
    , numberedTilesSelector_ = "Fichas numeradas"
    , numberedTilesSelectorNo_ = "No"
    , numberedTilesSelectorYes_ = "Sí"
    , pinTileDescription_ = \number -> number ++ " de círculos"
    , redDragonDescription_ = "dragón rojo"
    , selectWaitTilesText_ = "Selecciona las esperas:"
    , settingsTitle_ = "Configuración"
    , showWaitTilesText_ = "Esperas:"
    , siteTitle_ = "Entrenador de esperas de Mahjong"
    , souTileDescription_ = \number -> number ++ " de bambú"
    , southWindDescription_ = "viento sur"
    , suitSelectorTitle_ = "Palo"
    , suitSelectorTitleMan_ = "Caracteres"
    , suitSelectorTitlePin_ = "Círculos"
    , suitSelectorTitleRandom_ = "Al azar"
    , suitSelectorTitleSou_ = "Bambúes"
    , tableTab_ = "Tabla"
    , themeSelectorTitle_ = "Tema"
    , themeSelectorTitleDark_ = "Oscuro"
    , themeSelectorTitleLight_ = "Claro"
    , tileNumber1_ = "uno"
    , tileNumber2_ = "dos"
    , tileNumber3_ = "tres"
    , tileNumber4_ = "cuatro"
    , tileNumber5_ = "cinco"
    , tileNumber6_ = "seis"
    , tileNumber7_ = "siete"
    , tileNumber8_ = "ocho"
    , tileNumber9_ = "nueve"
    , westWindDescription_ = "viento oeste"
    , whiteDragonDescription_ = "dragón blanco"
    , wrongAnswer_ = "Incorrecto"
    }


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
