module Main exposing (main)
import Browser
import Html exposing (Html, div, text, input, img, p, span)
import Html.Events exposing (onInput)
import Html.Attributes exposing (type_, placeholder, value, src)
import Parser exposing (Parser, (|.), (|=), succeed, oneOf, loop, getChompedString, chompIf, chompWhile)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view}


type alias Model = { hand: String }

type Suit = Sou | Man | Pin | Invalid
type alias Tile =
    { number: Int
    , suit: Suit }

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
    let
        tiles = showParseResult model.hand
    in 
    div []
        [ input [ type_ "text", placeholder "Hand", value model.hand, onInput Hand] []
        , p [] [ Debug.toString tiles |> text ]
        , p [] [ renderTiles tiles ]
        ]


drawTile: Tile -> Html Msg
drawTile tile =
    let
        n = String.fromInt tile.number
    in
    if tile.number == 0 then
        case tile.suit of
            Sou ->
                img [src ("/img/red-doras/red-dora-bamboo5.png")] []
            Pin ->
                img [src ("/img/red-doras/red-dora-pin5.png")] []
            Man ->
                img [src ("/img/red-doras/red-dora-man5.png")] []
            _ -> text ""
    else
        case tile.suit of
            Sou ->
                img [src ("/img/bamboo/bamboo" ++ n ++ ".png")] []
            Pin ->
                img [src ("/img/pin/pin" ++ n ++ ".png")] []
            Man ->
                img [src ("/img/man/man" ++ n ++ ".png")] []
            _ -> text ""


renderTiles: List Tile -> Html Msg
renderTiles tiles =
    span [] (List.map drawTile tiles)

toSuit : String -> Suit
toSuit s =
    case s of
        "p" -> Pin
        "s" -> Sou
        "m" -> Man
        _ -> Invalid

tilesFromSuitString : String -> List Tile
tilesFromSuitString parsedSuit =
    let
        suit = String.right 1 parsedSuit |> toSuit
        tiles = String.dropRight 1 parsedSuit
            |> String.toList
            |> List.map String.fromChar
            |> List.filterMap String.toInt
        
    in
    List.map (\n ->  Tile n suit) tiles


handSuit : Parser (List Tile)
handSuit =
    Parser.map tilesFromSuitString  <|
        getChompedString <|
            succeed ()
                |. chompWhile (\c -> Char.isDigit c)
                |. chompIf (\c -> c == 's' || c == 'm' || c == 'p')

parseHandHelper : List Tile -> Parser (Parser.Step (List Tile) (List Tile))
parseHandHelper parsedSuits =
    oneOf
        [ succeed (\hand -> Parser.Loop (List.append parsedSuits hand))
            |= handSuit
        , succeed ()
            |> Parser.map (\_ -> Parser.Done parsedSuits)
        ]

handSuits : Parser (List Tile)
handSuits =
    loop [] parseHandHelper

showParseResult: String -> List Tile
showParseResult input =
    case Parser.run handSuits input of
        Ok value -> value
        Err _ -> []


