module Page.Scoring exposing (Model, Msg, init, update, view)

import Browser
import Group
import Hand exposing (Hand)
import Html exposing (Html, a, button, div, input, li, p, table, tbody, td, text, tfoot, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import I18n
import Page.Debugger
import Random
import Tile
import UI


type alias Model =
    { i18n : I18n.I18n
    , handString : String
    , hand : Hand
    , allGroups : Group.GroupsBreakdown
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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = \m -> div [ class "container" ] [ view m ]
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { i18n = I18n.init I18n.En
      , handString = ""
      , hand = Hand.init
      , allGroups = { chiitoitsu = [], perSuit = Group.GroupsPerSuit [ [] ] [ [] ] [ [] ] [ [] ] }
      , activeTab = GuessTab
      , guessedValue = guessValueInit
      }
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
                    Tile.fromString handString

                allGroups =
                    Group.findGroups Group.SkipPartials tiles

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
                    Group.findGroups Group.SkipPartials newHand.tiles
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
                    Group.findGroups Group.SkipPartials newHand.tiles
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
    div []
        [ input [ class "input", type_ "text", placeholder "Hand", value model.handString, onInput HandStr ] []
        , button [ class "button is-primary", onClick GenerateRandomWinningHand ] [ text "Random winning hand" ]
        , p [] [ UI.tilesDivMinWidth model.i18n True model.hand.tiles ]
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
                in
                table [ class "table is-striped is-fullwidth" ]
                    [ tbody []
                        (List.map
                            (\( t, g ) ->
                                tr []
                                    [ td [] [ UI.tilesDivMinWidth model.i18n False [ t ] ]
                                    , td [] [ UI.groups model.i18n True t g ]
                                    ]
                            )
                            winningTiles
                        )
                    ]

            else
                div []
                    [ Page.Debugger.debugGroups model.allGroups
                    , UI.groupsSimple model.i18n True model.hand.groups
                    , renderHanDetails model.hand
                    , renderFuDetails model.i18n model.hand
                    , renderScore model.hand
                    ]


renderHanDetails : Hand -> Html Msg
renderHanDetails hand =
    let
        details =
            List.map renderHanSource hand.hanSources

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


renderFuDetails : I18n.I18n -> Hand -> Html Msg
renderFuDetails i18n hand =
    if Hand.shouldCountFu hand then
        let
            details =
                List.map (renderFuSource i18n) hand.fuSources

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


renderFuSource : I18n.I18n -> Hand.FuSource -> Html Msg
renderFuSource i18n fuSource =
    let
        explanation =
            Hand.fuDescriptionToString fuSource.description
    in
    tr []
        [ td [] [ text explanation ]
        , td [] [ text (String.fromInt fuSource.fu ++ " fu") ]
        , td [] [ UI.groupsSimple i18n True fuSource.groups ]
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
        [ p [ onClick ChangeSeatWind ] [ text ("Seat wind: " ++ Tile.windToString hand.seatWind) ]
        , p [ onClick ChangeRoundWind ] [ text ("Round wind: " ++ Tile.windToString hand.roundWind) ]
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
                String.concat
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
            [ text "Select han count:"
            , div [ class "buttons has-addons" ] hanButtons
            ]

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
            [ text "Select fu count:"
            , div [ class "buttons has-addons" ] buttons
            ]

        fuSummary =
            if isFuGuessed model.guessedValue then
                renderFuDetails model.i18n model.hand

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
            div [ class "field has-addons" ]
                [ div [ class "control" ] [ input [ class ("input " ++ inputClass), type_ "text", placeholder "Input score", onInput GuessedScoreString ] [] ]
                , div [ class "control" ] [ button [ class "button is-primary", onClick SetGuessedScore ] [ text "Guess score" ] ]
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
