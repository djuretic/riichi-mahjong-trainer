module Main exposing (findWinningHand, main, showParseResult)

import Browser
import Hand exposing (FuSource, Hand, Yaku(..), checkAllYaku, countFu, fuDescriptionToString)
import Html exposing (Html, button, div, input, li, p, table, td, text, tr, ul)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import Parser exposing ((|.), (|=), Parser, chompIf, chompWhile, getChompedString, loop, oneOf, succeed)
import Random
import Tile exposing (Group, GroupType(..), GroupsPerSuit, Suit(..), Tile, findGroups, groupToString, suitToString, windToString)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { handString : String
    , hand : Hand
    , allGroups : GroupsPerSuit
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "2555m" Hand.init (GroupsPerSuit [ [] ] [ [] ] [ [] ] [ [] ])
    , Cmd.none
    )


type Msg
    = HandStr String
    | ChangeSeatWind
    | ChangeRoundWind
    | ChangeWinBy
    | GenerateRandomHand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandStr handString ->
            let
                tiles =
                    showParseResult handString

                allGroups =
                    findGroups tiles

                groups =
                    findWinningHand allGroups

                prevHand =
                    model.hand

                hand =
                    -- keep the older winds
                    { prevHand
                        | tiles = tiles
                        , winBy = Hand.Tsumo
                        , groups = groups
                        , han = []
                        , fu = []
                    }

                allYaku =
                    checkAllYaku hand

                handWithYaku =
                    { hand | han = allYaku }
            in
            ( { model | handString = handString, hand = handWithYaku, allGroups = allGroups }, Cmd.none )

        ChangeRoundWind ->
            let
                prevHand =
                    model.hand

                newHand =
                    { prevHand | roundWind = cycleWind prevHand.roundWind }
            in
            ( { model | hand = newHand }, Cmd.none )

        ChangeSeatWind ->
            let
                prevHand =
                    model.hand

                newHand =
                    { prevHand | seatWind = cycleWind prevHand.seatWind }
            in
            ( { model | hand = newHand }, Cmd.none )

        ChangeWinBy ->
            let
                prevHand =
                    model.hand

                newWinBy =
                    if model.hand.winBy == Hand.Ron then
                        Hand.Tsumo

                    else
                        Hand.Ron

                newHand =
                    { prevHand | winBy = newWinBy }
            in
            ( { model | hand = newHand }, Cmd.none )

        GenerateRandomHand ->
            let
                randomHand =
                    randomWinningHand
                        |> Random.map (\lg -> List.map groupToString lg |> String.join "")
            in
            ( model
            , Random.generate HandStr randomHand
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        handWithFu =
            countFu model.hand
    in
    div []
        [ input [ type_ "text", placeholder "Hand", value model.handString, onInput HandStr ] []
        , button [ onClick GenerateRandomHand ] [ text "Random" ]

        -- , p [] [ Debug.toString tiles |> text ]
        , p [] [ renderTiles model.hand.tiles ]
        , renderWinBy model.hand
        , renderWinds model.hand
        , debugGroups model.allGroups
        , drawGroups model.hand.groups
        , p [] [ text <| "Yaku: " ++ Debug.toString model.hand.han ]

        --, p [] [text (Debug.toString model.hand.groups), clearFixDiv]
        , clearFixDiv
        , renderFuDetails handWithFu.fu
        ]


drawTile : Tile -> Html Msg
drawTile tile =
    let
        n =
            String.fromInt tile.number

        isRedDora =
            tile.number == 0

        path =
            if isRedDora then
                case tile.suit of
                    Sou ->
                        "/img/red-doras/red-dora-bamboo5.png"

                    Pin ->
                        "/img/red-doras/red-dora-pin5.png"

                    Man ->
                        "/img/red-doras/red-dora-man5.png"

                    Honor ->
                        ""

            else
                case tile.suit of
                    Sou ->
                        "/img/bamboo/bamboo" ++ n ++ ".png"

                    Pin ->
                        "/img/pin/pin" ++ n ++ ".png"

                    Man ->
                        "/img/man/man" ++ n ++ ".png"

                    Honor ->
                        pathHonorTile tile.number
    in
    if String.isEmpty path then
        text ""

    else
        div
            [ style "background-image" ("url(" ++ path ++ ")")
            , style "background-position-x" "-10px"
            , style "float" "left"
            , style "height" "64px"
            , style "width" "45px"
            ]
            []


pathHonorTile : Int -> String
pathHonorTile n =
    case n of
        1 ->
            "/img/winds/wind-east.png"

        2 ->
            "/img/winds/wind-south.png"

        3 ->
            "/img/winds/wind-west.png"

        4 ->
            "/img/winds/wind-north.png"

        5 ->
            "/img/dragons/dragon-haku.png"

        6 ->
            "/img/dragons/dragon-green.png"

        7 ->
            "/img/dragons/dragon-chun.png"

        _ ->
            ""


clearFixDiv : Html msg
clearFixDiv =
    div [ style "clear" "both" ] []


renderTiles : List Tile -> Html Msg
renderTiles tiles =
    div [] (List.append (List.map drawTile tiles) [ clearFixDiv ])


toSuit : String -> Maybe Suit
toSuit s =
    case s of
        "p" ->
            Just Pin

        "s" ->
            Just Sou

        "m" ->
            Just Man

        "z" ->
            Just Honor

        _ ->
            Nothing


tilesFromSuitString : String -> List Tile
tilesFromSuitString parsedSuit =
    let
        suit =
            String.right 1 parsedSuit |> toSuit

        tiles =
            String.dropRight 1 parsedSuit
                |> String.toList
                |> List.map String.fromChar
                |> List.filterMap String.toInt
    in
    case suit of
        Just s ->
            List.map (\n -> Tile n s) tiles

        Nothing ->
            []


handSuit : Parser (List Tile)
handSuit =
    Parser.map tilesFromSuitString <|
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


showParseResult : String -> List Tile
showParseResult input =
    case Parser.run handSuits input of
        Ok value ->
            value

        Err _ ->
            []


findWinningHand : GroupsPerSuit -> List Group
findWinningHand groups =
    let
        firstItem =
            \g -> Maybe.withDefault [] (List.head g)

        man =
            firstItem groups.man

        pin =
            firstItem groups.pin

        sou =
            firstItem groups.sou

        honor =
            firstItem groups.honor

        possibleGroups =
            List.concat [ man, pin, sou, honor ]

        numberPairs =
            List.filter (\g -> g.type_ == Pair) possibleGroups |> List.length

        groupSort g =
            ( suitToString g.suit, g.tileNumber )
    in
    if List.length possibleGroups == 5 && numberPairs == 1 then
        List.sortBy groupSort possibleGroups

    else
        []


drawGroup : Group -> Html Msg
drawGroup group =
    div []
        (List.map drawTile (groupToTiles group))


groupToTiles : Group -> List Tile
groupToTiles group =
    case group.type_ of
        Pair ->
            List.repeat 2 (Tile group.tileNumber group.suit)

        Triplet ->
            List.repeat 3 (Tile group.tileNumber group.suit)

        Run ->
            [ Tile group.tileNumber group.suit
            , Tile (group.tileNumber + 1) group.suit
            , Tile (group.tileNumber + 2) group.suit
            ]


drawGroups : List Group -> Html Msg
drawGroups groups =
    div [] (List.map drawGroup groups)


debugGroup : List Group -> Html Msg
debugGroup listGroup =
    if List.isEmpty listGroup then
        text "[]"

    else
        ul [] (List.map (\g -> li [] [ text (Debug.toString g) ]) listGroup)


debugGroups : GroupsPerSuit -> Html Msg
debugGroups groups =
    let
        cellStyle =
            style "border" "1px solid black"

        generateTd l =
            List.map (\g -> td [ cellStyle ] [ debugGroup g ]) l
    in
    table []
        [ tr [] [ text "groupsPerSuit" ]
        , tr [] (generateTd groups.man)
        , tr [] (generateTd groups.pin)
        , tr [] (generateTd groups.sou)
        , tr [] (generateTd groups.honor)
        ]


renderFuDetails : List FuSource -> Html Msg
renderFuDetails fuSources =
    table [] <|
        List.concat [ List.map renderFuSource fuSources, renderTotalFu fuSources ]


renderFuSource : FuSource -> Html Msg
renderFuSource fuSource =
    let
        explanation =
            fuDescriptionToString fuSource.description
    in
    tr []
        [ td [] [ text explanation ]
        , td [] [ text (String.fromInt fuSource.fu ++ " fu") ]
        , td [] [ drawGroups fuSource.groups ]
        ]


renderTotalFu : List FuSource -> List (Html Msg)
renderTotalFu fuSources =
    let
        sumFu =
            List.map .fu fuSources
                |> List.sum

        roundFu n =
            toFloat n
                / 10
                |> ceiling
                |> (*) 10
    in
    [ tr []
        [ td [] [ text "Total (before rounding)" ]
        , td [] [ text <| String.fromInt sumFu ++ " fu" ]
        ]
    , tr []
        [ td [] [ text "Total" ]
        , td [] [ text <| String.fromInt (roundFu sumFu) ++ " fu" ]
        ]
    ]


renderWinBy : Hand -> Html Msg
renderWinBy hand =
    div []
        [ p [ onClick ChangeWinBy ] [ text <| "Win by: " ++ Hand.winByToString hand.winBy ] ]


renderWinds : Hand -> Html Msg
renderWinds hand =
    div []
        [ p [ onClick ChangeSeatWind ] [ text ("Seat wind: " ++ windToString hand.seatWind) ]
        , p [ onClick ChangeRoundWind ] [ text ("Round wind: " ++ windToString hand.roundWind) ]
        ]


cycleWind : Tile.Wind -> Tile.Wind
cycleWind wind =
    case wind of
        Tile.East ->
            Tile.South

        Tile.South ->
            Tile.West

        Tile.West ->
            Tile.North

        Tile.North ->
            Tile.East


randomTripletOrRun : Suit -> Random.Generator Group
randomTripletOrRun suit =
    let
        maxRange =
            if suit == Honor then
                7

            else
                9 + 7
    in
    Random.int 1 maxRange
        |> Random.map
            (\n ->
                if n < 10 then
                    Group Triplet n suit

                else
                    Group Run (n - 9) suit
            )


randomPair : Random.Generator Group
randomPair =
    let
        maxRange suit =
            if suit == Honor then
                7

            else
                9
    in
    Random.uniform Man [ Pin, Sou, Honor ]
        |> Random.andThen (\s -> Random.pair (Random.int 1 (maxRange s)) (Random.constant s))
        |> Random.map (\( n, suit ) -> Group Pair n suit)


randomWinningHand : Random.Generator (List Group)
randomWinningHand =
    let
        man =
            randomTripletOrRun Man

        pin =
            randomTripletOrRun Pin

        sou =
            randomTripletOrRun Sou

        honor =
            randomTripletOrRun Honor

        pair =
            randomPair
    in
    Random.map5 (\m p s h pp -> [ m, p, s, h, pp ]) man pin sou honor pair
