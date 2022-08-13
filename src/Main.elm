module Main exposing (findWinningHand, main, showParseResult)

import Browser
import Group exposing (Group, GroupType(..), GroupsPerSuit)
import Hand exposing (FuSource, Hand, Yaku(..), fuDescriptionToString)
import Html exposing (Html, a, button, div, h1, input, li, node, p, table, tbody, td, text, tfoot, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, colspan, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import Parser exposing ((|.), (|=), Parser, chompIf, chompWhile, getChompedString, loop, oneOf, succeed)
import Random
import Tile exposing (Suit(..), Tile, suitToString, windToString)


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
    , guessState : GuessState
    , inputGuessScore : String
    }


type ActiveTab
    = GuessTab
    | CountSummaryTab


type GuessState
    = InitialGuess
    | SelectedHanCount Int
    | SelectedHanAndFuCount Int Int
    | SelectedHanFuCountAndScore Int Int Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Hand.init (GroupsPerSuit [ [] ] [ [] ] [ [] ] [ [] ]) GuessTab InitialGuess ""
    , Random.generate HandGenerated Hand.randomWinningHand
    )


type Msg
    = HandStr String
    | HandGenerated Hand
    | ChangeSeatWind
    | ChangeRoundWind
    | ChangeWinBy
    | GenerateRandomHand
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
                    findWinningHand allGroups

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
            ( { model | handString = handString, hand = Hand.count hand, allGroups = allGroups, guessState = InitialGuess }, Cmd.none )

        HandGenerated hand ->
            let
                excessRepeatedTiles =
                    Tile.hasMoreThan4Tiles hand.tiles

                newHand =
                    Hand.count hand

                allGroups =
                    Group.findGroups newHand.tiles
            in
            if newHand.hanCount > 0 && not excessRepeatedTiles then
                ( { model | handString = Hand.getHandString newHand, hand = newHand, allGroups = allGroups, guessState = InitialGuess }, Cmd.none )

            else
                update GenerateRandomHand model

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

        GenerateRandomHand ->
            ( model
            , Random.generate HandGenerated Hand.randomWinningHand
            )

        ChangeTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        SetGuessedHan han ->
            ( { model | guessState = SelectedHanCount han }, Cmd.none )

        SetGuessedFu fu ->
            let
                nextState =
                    case model.guessState of
                        InitialGuess ->
                            InitialGuess

                        SelectedHanCount han ->
                            SelectedHanAndFuCount han fu

                        SelectedHanAndFuCount han _ ->
                            SelectedHanAndFuCount han fu

                        SelectedHanFuCountAndScore han _ score ->
                            SelectedHanFuCountAndScore han fu score
            in
            ( { model | guessState = nextState }, Cmd.none )

        GuessedScoreString str ->
            ( { model | inputGuessScore = str }, Cmd.none )

        SetGuessedScore ->
            let
                score =
                    String.toInt model.inputGuessScore
                        |> Maybe.withDefault 0

                nextState =
                    case model.guessState of
                        InitialGuess ->
                            InitialGuess

                        SelectedHanCount _ ->
                            model.guessState

                        SelectedHanAndFuCount han fu ->
                            SelectedHanFuCountAndScore han fu score

                        SelectedHanFuCountAndScore han fu _ ->
                            SelectedHanFuCountAndScore han fu score
            in
            ( { model | guessState = nextState }, Cmd.none )


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

        renderTabContent : ActiveTab -> Html Msg
        renderTabContent tab =
            case tab of
                GuessTab ->
                    renderGuessTab model

                CountSummaryTab ->
                    div []
                        [ debugGroups model.allGroups
                        , drawGroups model.hand.groups
                        , clearFixDiv
                        , renderHanDetails model.hand
                        , renderFuDetails model.hand
                        , renderScore model.hand
                        ]
    in
    div [ class "container" ]
        [ stylesheet
        , h1 [ class "title" ] [ text "Riichi mahjong trainer" ]
        , input [ class "input", type_ "text", placeholder "Hand", value model.handString, onInput HandStr ] []
        , button [ class "button is-primary", onClick GenerateRandomHand ] [ text "Random" ]
        , p [] [ renderTiles model.hand.tiles ]
        , renderWinBy model.hand
        , renderWinds model.hand
        , div [ class "tabs" ]
            [ ul []
                [ li (tabAttrs GuessTab model.activeTab) [ a [] [ text "Score" ] ]
                , li (tabAttrs CountSummaryTab model.activeTab) [ a [] [ text "Count summary" ] ]
                ]
            ]
        , renderTabContent model.activeTab
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
        , td [] [ drawGroups fuSource.groups ]
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
                , td [] [ text (String.fromInt scoreCell.dealer.ron) ]
                , td [] [ text (String.fromInt scoreCell.dealer.tsumo) ]
                ]
            , tr []
                [ th [] [ text "Non-dealer" ]
                , td [] [ text (String.fromInt scoreCell.nonDealer.ron) ]
                , td [] [ text (String.fromInt scoreCell.nonDealer.tsumoDealer ++ " / " ++ String.fromInt scoreCell.nonDealer.tsumoNonDealer) ]
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
        ( guessedHan, guessedFu, guessedScore ) =
            case model.guessState of
                InitialGuess ->
                    ( Nothing, Nothing, Nothing )

                SelectedHanCount han ->
                    ( Just han, Nothing, Nothing )

                SelectedHanAndFuCount han fu ->
                    ( Just han, Just fu, Nothing )

                SelectedHanFuCountAndScore han fu score ->
                    ( Just han, Just fu, Just score )

        hanButtons =
            List.range 1 13
                |> List.map (guessHanButton model guessedHan)

        hanButtonSection =
            List.append [ text "Select han count:" ] hanButtons

        hanSummary =
            if isHanGuessed model.guessState then
                renderHanDetails model.hand

            else
                div [] []

        fuButtonSection =
            let
                buttons =
                    List.range 2 7
                        |> List.map ((*) 10)
                        |> List.map (guessFuButton model guessedFu)
            in
            if Hand.shouldCountFu model.hand then
                List.append [ text "Select fu count:" ] buttons

            else
                []

        fuSummary =
            if isFuGuessed model.guessState then
                renderFuDetails model.hand

            else
                div [] []

        scoreInput =
            let
                realScore =
                    Hand.score model.hand

                inputClass =
                    if isScoreGuessed model.guessState then
                        if realScore == Maybe.withDefault 0 guessedScore then
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
            if isScoreGuessed model.guessState then
                renderScore model.hand

            else
                div [] []
    in
    div []
        ((hanButtonSection ++ [ hanSummary ]) ++ fuButtonSection ++ [ fuSummary, scoreInput, scoreSummary ])


isHanGuessed : GuessState -> Bool
isHanGuessed guessState =
    case guessState of
        InitialGuess ->
            False

        _ ->
            True


isFuGuessed : GuessState -> Bool
isFuGuessed guessState =
    case guessState of
        SelectedHanAndFuCount _ _ ->
            True

        SelectedHanFuCountAndScore _ _ _ ->
            True

        _ ->
            False


isScoreGuessed : GuessState -> Bool
isScoreGuessed guessState =
    case guessState of
        SelectedHanFuCountAndScore _ _ _ ->
            True

        _ ->
            False


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
