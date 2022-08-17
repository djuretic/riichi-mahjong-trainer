module Main exposing (main, showParseResult)

import Browser
import Group exposing (Group, GroupType(..), GroupsPerSuit)
import Hand exposing (FuSource, Hand, Yaku(..), fuDescriptionToString)
import Html exposing (Html, a, button, div, h1, input, li, node, p, table, tbody, td, text, tfoot, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, colspan, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Maybe
import Parser exposing ((|.), (|=), Parser, chompIf, chompWhile, getChompedString, loop, oneOf, succeed)
import Random
import Tile exposing (Suit(..), Tile, windToString)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


stylesheet : Html Msg
stylesheet =
    let
        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "../css/bulma.min.css"
            ]
    in
    node "link" attrs []


type alias Model =
    { handString : String
    , hand : Hand
    , allGroups : GroupsPerSuit
    , activeTab : ActiveTab
    , guessedValue : GuessValue
    }


type alias GuessValue =
    { han : Maybe Int
    , fu : Maybe Int
    , score : Maybe Int
    , inputScore : String
    }


type ActiveTab
    = GuessTab
    | SummaryTab


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Hand.init (GroupsPerSuit [ [] ] [ [] ] [ [] ] [ [] ]) GuessTab guessValueInit
    , Random.generate WinningHandGenerated Hand.randomWinningHand
    )


guessValueInit : GuessValue
guessValueInit =
    { han = Nothing
    , fu = Nothing
    , score = Nothing
    , inputScore = ""
    }


type Msg
    = HandStr String
    | WinningHandGenerated Hand
    | TenpaiHandGenerated Hand
    | ChangeSeatWind
    | ChangeRoundWind
    | ChangeWinBy
    | GenerateRandomWinningHand
    | GenerateRandomTenpaiHand
    | ChangeTab ActiveTab
    | SetGuessedHan Int
    | SetGuessedFu Int
    | GuessedScoreString String
    | SetGuessedScore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandStr handString ->
            let
                tiles =
                    showParseResult handString

                allGroups =
                    Group.findGroups tiles

                groups =
                    Group.findWinningGroups allGroups

                prevHand =
                    model.hand

                hand =
                    -- keep the older winds
                    { prevHand
                        | tiles = tiles
                        , winBy = Hand.Tsumo
                        , groups = groups
                        , hanSources = []
                        , fuSources = []
                    }
            in
            ( { model | handString = handString, hand = Hand.count hand, allGroups = allGroups, guessedValue = guessValueInit }, Cmd.none )

        WinningHandGenerated hand ->
            let
                newHand =
                    Hand.count hand

                allGroups =
                    Group.findGroups newHand.tiles
            in
            if Hand.isWinningHand newHand then
                ( { model | handString = Hand.getHandString newHand, hand = newHand, allGroups = allGroups, guessedValue = guessValueInit }, Cmd.none )

            else
                update GenerateRandomWinningHand model

        TenpaiHandGenerated hand ->
            let
                newHand =
                    Hand.count hand

                allGroups =
                    Group.findGroups newHand.tiles
            in
            ( { model | handString = Hand.getHandString newHand, hand = newHand, allGroups = allGroups, guessedValue = guessValueInit }, Cmd.none )

        ChangeRoundWind ->
            let
                prevHand =
                    model.hand

                newHand =
                    { prevHand | roundWind = cycleWind prevHand.roundWind }
            in
            ( { model | hand = Hand.count newHand }, Cmd.none )

        ChangeSeatWind ->
            let
                prevHand =
                    model.hand

                newHand =
                    { prevHand | seatWind = cycleWind prevHand.seatWind }
            in
            ( { model | hand = Hand.count newHand }, Cmd.none )

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
                    Hand.count { prevHand | winBy = newWinBy }
            in
            ( { model | hand = newHand }, Cmd.none )

        GenerateRandomWinningHand ->
            ( model
            , Random.generate WinningHandGenerated Hand.randomWinningHand
            )

        GenerateRandomTenpaiHand ->
            ( model
            , Random.generate TenpaiHandGenerated Hand.randomTenpaiHand
            )

        ChangeTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        SetGuessedHan han ->
            let
                value =
                    model.guessedValue
            in
            ( { model | guessedValue = { value | han = Just han } }, Cmd.none )

        SetGuessedFu fu ->
            let
                value =
                    model.guessedValue
            in
            ( { model | guessedValue = { value | fu = Just fu } }, Cmd.none )

        GuessedScoreString str ->
            let
                value =
                    model.guessedValue
            in
            ( { model | guessedValue = { value | inputScore = str } }, Cmd.none )

        SetGuessedScore ->
            let
                score =
                    String.toInt model.guessedValue.inputScore
                        |> Maybe.withDefault 0

                value =
                    model.guessedValue
            in
            ( { model | guessedValue = { value | score = Just score } }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        tabAttrs : ActiveTab -> ActiveTab -> List (Html.Attribute Msg)
        tabAttrs tab currentTab =
            if tab == currentTab then
                [ class "is-active", onClick (ChangeTab tab) ]

            else
                [ onClick (ChangeTab tab) ]
    in
    div [ class "container" ]
        [ stylesheet
        , h1 [ class "title" ] [ text "Riichi mahjong trainer" ]
        , input [ class "input", type_ "text", placeholder "Hand", value model.handString, onInput HandStr ] []
        , button [ class "button is-primary", onClick GenerateRandomWinningHand ] [ text "Random winning hand" ]
        , button [ class "button is-primary", onClick GenerateRandomTenpaiHand ] [ text "Random tenpai hand" ]
        , p [] [ renderTiles True model.hand.tiles ]
        , renderWinBy model.hand
        , renderWinds model.hand
        , div [ class "tabs" ]
            [ ul []
                [ li (tabAttrs GuessTab model.activeTab) [ a [] [ text "Guess" ] ]
                , li (tabAttrs SummaryTab model.activeTab) [ a [] [ text "Summary" ] ]
                ]
            ]
        , renderTabContent model
        ]


renderTabContent : Model -> Html Msg
renderTabContent model =
    case model.activeTab of
        GuessTab ->
            renderGuessTab model

        SummaryTab ->
            if List.length model.hand.tiles == 13 then
                let
                    winningTiles =
                        Hand.winningTiles model.hand

                    commonGroups =
                        Group.commonGroups (List.map Tuple.second winningTiles)
                in
                table [ class "table is-striped is-fullwidth" ]
                    [ tbody []
                        (List.map
                            (\( t, g ) ->
                                tr []
                                    [ td [] [ renderTiles False [ t ] ]
                                    , td [] [ drawGroups commonGroups g ]
                                    ]
                            )
                            winningTiles
                        )
                    ]

            else
                div []
                    [ debugGroups model.allGroups
                    , drawGroupsSimple model.hand.groups
                    , renderHanDetails model.hand
                    , renderFuDetails model.hand
                    , renderScore model.hand
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
        div (tileCss path) []


drawBackTile : Html Msg
drawBackTile =
    div (tileCss "/img/face-down-64px.png") []


tileCss : String -> List (Html.Attribute msg)
tileCss path =
    [ style "background-image" ("url(" ++ path ++ ")")
    , style "background-position-x" "-10px"
    , style "height" "64px"
    , style "width" "45px"
    ]


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


renderTiles : Bool -> List Tile -> Html Msg
renderTiles addEmptySpots tiles =
    let
        renderedTiles =
            List.map drawTile tiles

        emptySpots =
            if addEmptySpots then
                List.repeat (14 - List.length renderedTiles) drawBackTile

            else
                []

        allTiles =
            List.append (List.map drawTile tiles) emptySpots
    in
    div [ class "is-flex is-flex-direction-row" ] allTiles


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


drawGroup : List (Html.Attribute Msg) -> Group -> Html Msg
drawGroup attrs group =
    div (List.append [ class "is-flex is-flex-direction-row", style "padding-right" "10px" ] attrs)
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


drawGroups : List Group -> List Group -> Html Msg
drawGroups specialGroups groups =
    let
        addGroupIsRepeatedData sg lg =
            case lg of
                [] ->
                    []

                x :: xs ->
                    if List.Extra.find (\e -> e == x) sg /= Nothing then
                        ( x, True ) :: addGroupIsRepeatedData (List.Extra.remove x sg) xs

                    else
                        ( x, False ) :: addGroupIsRepeatedData sg xs

        groupsWithRepeatedInfo =
            addGroupIsRepeatedData specialGroups groups

        css isRepeated =
            if isRepeated then
                style "opacity" "0.5"

            else
                class ""
    in
    div [ class "is-flex is-flex-direction-row is-flex-wrap-wrap" ]
        (List.map (\( g, isRepeated ) -> drawGroup [ css isRepeated ] g) groupsWithRepeatedInfo)


drawGroupsSimple : List Group -> Html Msg
drawGroupsSimple groups =
    div [ class "is-flex is-flex-direction-row is-flex-wrap-wrap" ] (List.map (drawGroup []) groups)


debugGroup : List Group -> Html Msg
debugGroup listGroup =
    if List.isEmpty listGroup then
        text "[]"

    else
        ul [] (List.map (\g -> li [] [ text (Debug.toString g) ]) listGroup)


debugGroups : GroupsPerSuit -> Html Msg
debugGroups groups =
    let
        generateTd l =
            List.map (\g -> td [] [ debugGroup g ]) l
    in
    table [ class "table is-striped" ]
        [ thead []
            [ tr [] [ th [] [ text "groupsPerSuit" ] ] ]
        , tbody []
            [ tr [] (generateTd groups.man)
            , tr [] (generateTd groups.pin)
            , tr [] (generateTd groups.sou)
            , tr [] (generateTd groups.honor)
            ]
        ]


renderHanDetails : Hand -> Html Msg
renderHanDetails hand =
    let
        details =
            List.concat [ List.map renderHanSource hand.hanSources ]

        footer =
            renderTotalHan hand.hanCount
    in
    table [ class "table is-striped" ]
        [ thead [] [ th [ colspan 3 ] [ text "Han" ] ]
        , tbody [] details
        , tfoot [] [ footer ]
        ]


renderTotalHan : Int -> Html Msg
renderTotalHan totalHan =
    tr []
        [ td [] [ text "Total" ]
        , td [] [ text <| String.fromInt totalHan ++ " han" ]
        , td [] []
        ]


renderHanSource : Hand.HanSource -> Html Msg
renderHanSource hanSource =
    let
        explanation =
            Hand.hanDescriptionToString hanSource.description
    in
    tr []
        [ td [] [ text explanation ]
        , td [] [ text (String.fromInt hanSource.han ++ " han") ]
        ]


renderFuDetails : Hand -> Html Msg
renderFuDetails hand =
    if Hand.shouldCountFu hand then
        let
            details =
                List.concat [ List.map renderFuSource hand.fuSources ]

            footer =
                renderTotalFu hand
        in
        table [ class "table is-striped" ]
            [ thead [] [ th [ colspan 3 ] [ text "Fu" ] ]
            , tbody [] details
            , tfoot [] footer
            ]

    else
        div [] []


renderFuSource : FuSource -> Html Msg
renderFuSource fuSource =
    let
        explanation =
            fuDescriptionToString fuSource.description
    in
    tr []
        [ td [] [ text explanation ]
        , td [] [ text (String.fromInt fuSource.fu ++ " fu") ]
        , td [] [ drawGroupsSimple fuSource.groups ]
        ]


renderTotalFu : Hand -> List (Html Msg)
renderTotalFu { fuCount, fuCountBeforeRounding } =
    [ tr []
        [ td [] [ text "Total (before rounding)" ]
        , td [] [ text <| String.fromInt fuCountBeforeRounding ++ " fu" ]
        , td [] []
        ]
    , tr []
        [ td [] [ text "Total" ]
        , td [] [ text <| String.fromInt fuCount ++ " fu" ]
        , td [] []
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


renderScore : Hand -> Html Msg
renderScore hand =
    let
        scoreCell =
            Hand.scoreCell hand

        hanStr =
            String.fromInt hand.hanCount ++ " han "

        title =
            if hand.hanCount >= 5 then
                hanStr

            else
                String.join ""
                    [ hanStr
                    , String.fromInt hand.fuCount
                    , " fu"
                    ]

        isRon =
            Hand.isRon hand

        isTsumo =
            Hand.isTsumo hand

        isDealer =
            Hand.isDealer hand

        selectIf cond =
            if cond then
                class "is-selected"

            else
                class ""
    in
    table [ class "table is-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text title ]
                , th [] [ text "Ron" ]
                , th [] [ text "Tsumo" ]
                ]
            ]
        , tbody []
            [ tr []
                [ th [] [ text "Dealer" ]
                , td [ selectIf (isRon && isDealer) ] [ text (String.fromInt scoreCell.dealer.ron) ]
                , td [ selectIf (isTsumo && isDealer) ] [ text (String.fromInt scoreCell.dealer.tsumo ++ " all") ]
                ]
            , tr []
                [ th [] [ text "Non-dealer" ]
                , td [ selectIf (isRon && not isDealer) ] [ text (String.fromInt scoreCell.nonDealer.ron) ]
                , td [ selectIf (isTsumo && not isDealer) ] [ text (String.fromInt scoreCell.nonDealer.tsumoDealer ++ " / " ++ String.fromInt scoreCell.nonDealer.tsumoNonDealer) ]
                ]
            ]
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


renderGuessTab : Model -> Html Msg
renderGuessTab model =
    let
        hanButtons =
            List.range 1 13
                |> List.map (guessHanButton model model.guessedValue.han)

        hanButtonSection =
            List.append [ text "Select han count:" ] hanButtons

        hanSummary =
            if isHanGuessed model.guessedValue then
                renderHanDetails model.hand

            else
                div [] []

        fuButtonSection =
            let
                buttons =
                    List.range 2 7
                        |> List.map ((*) 10)
                        |> List.map (guessFuButton model model.guessedValue.fu)
            in
            if Hand.shouldCountFu model.hand then
                List.append [ text "Select fu count:" ] buttons

            else
                []

        fuSummary =
            if isFuGuessed model.guessedValue then
                renderFuDetails model.hand

            else
                div [] []

        scoreInput =
            let
                realScore =
                    Hand.score model.hand

                inputClass =
                    if isScoreGuessed model.guessedValue then
                        if realScore == Maybe.withDefault 0 model.guessedValue.score then
                            "is-success"

                        else
                            "is-danger"

                    else
                        ""
            in
            div []
                [ input [ class ("input " ++ inputClass), type_ "text", placeholder "Input score", onInput GuessedScoreString ] []
                , button [ class "button is-primary", onClick SetGuessedScore ] [ text "Guess score" ]
                ]

        scoreSummary =
            if isScoreGuessed model.guessedValue then
                renderScore model.hand

            else
                div [] []
    in
    div []
        ((hanButtonSection ++ [ hanSummary ]) ++ fuButtonSection ++ [ fuSummary, scoreInput, scoreSummary ])


isHanGuessed : GuessValue -> Bool
isHanGuessed value =
    value.han /= Nothing


isFuGuessed : GuessValue -> Bool
isFuGuessed value =
    value.fu /= Nothing


isScoreGuessed : GuessValue -> Bool
isScoreGuessed value =
    value.score /= Nothing


guessButtonCss : Int -> Int -> Int -> Maybe String
guessButtonCss buttonValue correctValue guess =
    if guess == buttonValue then
        if guess == correctValue then
            Just "is-success"

        else
            Just "is-danger"

    else
        Nothing


guessHanButton : Model -> Maybe Int -> Int -> Html Msg
guessHanButton model guessedHan n =
    let
        buttonClass nn realCount =
            guessedHan
                |> Maybe.andThen (guessButtonCss nn realCount)
                |> Maybe.withDefault ""
    in
    button [ class ("button " ++ buttonClass n model.hand.hanCount), onClick (SetGuessedHan n) ] [ text (String.fromInt n ++ " han") ]


guessFuButton : Model -> Maybe Int -> Int -> Html Msg
guessFuButton model guessedFu n =
    let
        buttonClass nn realCount =
            guessedFu
                |> Maybe.andThen (guessButtonCss nn realCount)
                |> Maybe.withDefault ""
    in
    button [ class ("button " ++ buttonClass n model.hand.fuCount), onClick (SetGuessedFu n) ] [ text (String.fromInt n ++ " fu") ]
