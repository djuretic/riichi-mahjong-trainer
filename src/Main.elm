module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text, input, img, p)
import Html.Events exposing (onInput)
import Html.Attributes exposing (type_, placeholder, value, src)
import Parser exposing (Parser, (|.), (|=), succeed, int, getChompedString, chompIf, chompWhile)

main = Browser.sandbox { init = init, update = update, view = view}


type alias Model = { hand: String }

init : Model
init = Model ""


type Msg = Hand String

update: Msg -> Model -> Model
update msg model =
    case msg of
        Hand hand ->
            { model | hand = hand }


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Hand", value model.hand, onInput Hand] []
        , p [] [ text (showParseResult model.hand) ]
        , p [] [ drawTiles (showParseResult model.hand) ]
        ]


drawTile: String -> String -> Html Msg
drawTile n suit =
    case suit of
        "s" ->
            img [src ("/img/bamboo/bamboo" ++ n ++ ".png")] []
        "p" ->
            img [src ("/img/pin/pin" ++ n ++ ".png")] []
        "m" ->
            img [src ("/img/man/man" ++ n ++ ".png")] []
        _ -> text ""


drawTiles: String -> Html Msg
drawTiles parsedHand =
    let
        suit = String.right 1 parsedHand
        tiles = List.map String.fromChar (String.toList (String.dropRight 1 parsedHand))
    in
        div [] (List.map (\t -> drawTile t suit) tiles)


handSuit : Parser String
handSuit =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> Char.isDigit c)
            |. chompIf (\c -> c == 's' || c == 'm' || c == 'p')

showParseResult: String -> String
showParseResult input =
    case Parser.run handSuit input of
        Ok value -> value
        Err msg -> "Error"
