module Main exposing (main)
import Browser
import Html exposing (Html, div, text, input, p, table, tr, td, ul, li)
import Html.Events exposing (onInput)
import Html.Attributes exposing (type_, placeholder, value, style)
import Parser exposing (Parser, (|.), (|=), succeed, oneOf, loop, getChompedString, chompIf, chompWhile)
import List.Extra exposing (permutations)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view}


type alias Model = { hand: String }

type Suit = Sou | Man | Pin | Honor | Invalid
type alias TileNumber = Int
type alias Tile =
    { number: TileNumber
    , suit: Suit }
type alias TilesPerSuit =
    { sou: List Tile
    , man: List Tile
    , pin: List Tile
    , honor: List Tile}
type alias GroupsPerSuit =
    { sou: List (List Group)
    , man: List (List Group)
    , pin: List (List Group)
    , honor: List (List Group)}
type GroupType = Triplet | Run
type alias Group =
    { type_: GroupType
    , tileNumbers: List TileNumber }

init : Model
init = Model "2555m"


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
        , p [] [ Debug.toString (partitionBySuit tiles) |> text]
        -- , p [] [ Debug.toString (findGroups tiles) |> text]
        , debugGroups (findGroups tiles)
        ]


drawTile: Tile -> Html Msg
drawTile tile =
    let
        n = String.fromInt tile.number
        isRedDora = tile.number == 0
        path = if isRedDora then
                case tile.suit of
                    Sou -> "/img/red-doras/red-dora-bamboo5.png"
                    Pin -> "/img/red-doras/red-dora-pin5.png"
                    Man ->  "/img/red-doras/red-dora-man5.png"
                    Honor -> ""
                    Invalid -> ""
            else
                case tile.suit of
                    Sou -> "/img/bamboo/bamboo" ++ n ++ ".png"
                    Pin -> "/img/pin/pin" ++ n ++ ".png"
                    Man -> "/img/man/man" ++ n ++ ".png"
                    Honor -> pathHonorTile tile.number
                    Invalid -> ""
    in
    if String.isEmpty path then
        text ""
    else 
        div [ style "background-image" ("url(" ++ path ++ ")")
            , style "background-position-x" "-10px"
            , style "float" "left"
            , style "height" "64px"
            , style "width" "45px"] []


pathHonorTile: Int -> String
pathHonorTile n = 
    case n of
        1 -> "/img/winds/wind-east.png"
        2 -> "/img/winds/wind-south.png"
        3 -> "/img/winds/wind-west.png"
        4 -> "/img/winds/wind-north.png"
        5 -> "/img/dragons/dragon-haku.png"
        6 -> "/img/dragons/dragon-green.png"
        7 -> "/img/dragons/dragon-chun.png"
        _ -> ""


clearFixDiv : Html msg
clearFixDiv = div [style "clear" "both"] []

renderTiles: List Tile -> Html Msg
renderTiles tiles =
    div [] (List.append (List.map drawTile tiles) [clearFixDiv])

toSuit : String -> Suit
toSuit s =
    case s of
        "p" -> Pin
        "s" -> Sou
        "m" -> Man
        "z" -> Honor
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
                |. chompIf (\c -> c == 's' || c == 'm' || c == 'p' || c == 'z')

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

isTriplet : List TileNumber -> Bool
isTriplet tiles =
    case tiles of
        x :: y :: [z] -> x == y  && y == z
        _ -> False


-- TODO: red dora
isRun : List TileNumber -> Bool
isRun tiles =
    case tiles of
        x :: y :: [z] ->
            x + 1 == y && y + 1 == z
        _ -> False


partitionBySuit : List Tile -> TilesPerSuit
partitionBySuit tiles =
    let
        (pin, rest) = List.partition (\t -> t.suit == Pin) tiles
        (sou, rest2) = List.partition (\t -> t.suit == Sou) rest
        (man, rest3) = List.partition (\t -> t.suit == Man) rest2
    in
    { sou = sou, man = man, pin = pin, honor = rest3}


findGroupsInSuit : List TileNumber -> Bool -> List Group
findGroupsInSuit tiles considerRuns =
    case tiles of
        x :: y :: z :: xs ->
            let
                candidate = [x, y, z]
            in
            if considerRuns && isRun candidate then
               Group Run candidate :: findGroupsInSuit xs considerRuns
            else if isTriplet candidate then
                Group Triplet candidate :: findGroupsInSuit xs considerRuns
            else
                []
        _ -> []


-- only works on sorted input
deduplicate : List a -> List a
deduplicate list =
    let
        helper accum previousElement remaining =
            case remaining of
                [] ->
                    accum
                first :: rest ->
                    if first == previousElement then
                        helper accum previousElement rest
                    else
                        helper (first :: accum) first rest
    in
    case list of
        [] ->
            []
        x :: xs ->
            x :: helper [] x xs


permutationsAndDedup : List TileNumber -> List (List TileNumber)
permutationsAndDedup tileNumbers = 
    let
        perms = permutations tileNumbers
        -- TODO remove permutations of 3, abc def == def abc
        sortedPerms = List.sort perms
    in
    deduplicate sortedPerms

findGroups : List Tile -> GroupsPerSuit
findGroups tiles =
    let
        part = partitionBySuit tiles
        findAllGroups = \t withRuns->
            List.map .number t
                |> permutationsAndDedup
                |> List.map (\p -> findGroupsInSuit p withRuns)
                |> List.sortBy (\g -> List.map .tileNumbers g)
                |> deduplicate
        groupsPerSuit = {
            sou = findAllGroups part.sou True
            , man = findAllGroups part.man True
            , pin = findAllGroups part.pin True
            , honor = findAllGroups part.honor False }
    in
    groupsPerSuit


debugGroup: List Group -> Html Msg
debugGroup listGroup =
    ul [] (List.map (\g -> li [] [text (Debug.toString g)]) listGroup)

debugGroups : GroupsPerSuit -> Html Msg
debugGroups groups =
    let
        cellStyle = style "border" "1px solid black"

    in
    table []
        [ tr [] 
            [ td [cellStyle] (List.map debugGroup groups.man)
            ]
        , tr [] 
            [ td [cellStyle] (List.map debugGroup groups.sou)
            ]
        ]
