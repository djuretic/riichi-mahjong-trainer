module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, p, table, td, text, tr, ul)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (permutations)
import Maybe
import Parser exposing ((|.), (|=), Parser, chompIf, chompWhile, getChompedString, loop, oneOf, succeed)
import Random


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


type Suit
    = Sou
    | Man
    | Pin
    | Honor


type alias TileNumber =
    Int


type alias Tile =
    { number : TileNumber
    , suit : Suit
    }


type alias TilesPerSuit =
    { sou : List Tile
    , man : List Tile
    , pin : List Tile
    , honor : List Tile
    }


type alias GroupsPerSuit =
    { sou : List (List Group)
    , man : List (List Group)
    , pin : List (List Group)
    , honor : List (List Group)
    }


type GroupType
    = Triplet
    | Run
    | Pair


type alias Group =
    { type_ : GroupType

    -- for runs, is the first (lowest) tile
    , tileNumber : TileNumber
    , suit : Suit
    }


type WinBy
    = Ron
    | Tsumo


type ValuePairBy
    = ByDragon
    | BySeatWind
    | ByRoundWind
    | BySeatAndRoundWind


type OpenClose
    = Open
    | Closed


type TripletKind
    = IsTerminal
    | IsHonor
    | HasNoValue


type FuDescription
    = BaseFu
    | TsumoNotPinfu
    | ClosedRon
    | ValuePair ValuePairBy
    | WaitFu WaitType
    | TripletFu OpenClose TripletKind
    | KanFu OpenClose TripletKind
    | NoFu


type Wind
    = East
    | South
    | West
    | North


type alias Hand =
    { tiles : List Tile
    , groups : List Group
    , winBy : WinBy
    , seatWind : Wind
    , roundWind : Wind
    , han : List HanSource
    , fu : List FuSource
    }


type alias FuSource =
    { fu : Int
    , description : FuDescription
    , groups : List Group
    }


type Yaku
    = Chiitoitsu
    | Pinfu
    | Iipeikou
    | Ryanpeikou
    | Yakuhai
    | Tanyao
    | SanshokuDoujun
    | Chanta
    | Toitoi
    | SanshokuDoukou
    | NoYaku


type alias HanSource =
    { han : Int
    , description : Yaku
    }


type WaitType
    = OpenWait
    | ClosedWait
    | EdgeWait
    | PairWait
    | NoWait


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "2555m" (Hand [] [] Tsumo East East [] []) (GroupsPerSuit [ [] ] [ [] ] [ [] ] [ [] ])
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
                        , winBy = Tsumo
                        , groups = groups
                        , han = []
                        , fu = []
                    }

                allYaku =
                    List.filter (\y -> not (y == noYaku)) [ checkIipeikou hand, checkTanyao hand, checkToitoi hand, checkChanta hand, checkSanshokuDoujun hand, checkSanshokuDoukou hand, checkPinfu hand ]
                        |> List.append (checkYakuhai hand)

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
                    if model.hand.winBy == Ron then
                        Tsumo

                    else
                        Ron

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


suitToString : Suit -> String
suitToString suit =
    case suit of
        Man ->
            "m"

        Pin ->
            "p"

        Sou ->
            "s"

        Honor ->
            "z"


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


isTriplet : List TileNumber -> Bool
isTriplet tiles =
    case tiles of
        x :: y :: [ z ] ->
            x == y && y == z

        _ ->
            False



-- TODO: red dora


isRun : List TileNumber -> Bool
isRun tiles =
    case tiles of
        x :: y :: [ z ] ->
            x + 1 == y && y + 1 == z

        _ ->
            False


partitionBySuit : List Tile -> TilesPerSuit
partitionBySuit tiles =
    let
        ( pin, rest ) =
            List.partition (\t -> t.suit == Pin) tiles

        ( sou, rest2 ) =
            List.partition (\t -> t.suit == Sou) rest

        ( man, rest3 ) =
            List.partition (\t -> t.suit == Man) rest2
    in
    { sou = sou, man = man, pin = pin, honor = rest3 }


findGroupsInSuit : List TileNumber -> Suit -> List Group
findGroupsInSuit tiles suit =
    case tiles of
        x :: y :: z :: xs ->
            let
                candidate =
                    [ x, y, z ]

                emptyRemaining =
                    List.isEmpty xs
            in
            if suit /= Honor && isRun candidate then
                let
                    rest =
                        findGroupsInSuit xs suit
                in
                if List.isEmpty rest && not emptyRemaining then
                    []

                else
                    Group Run x suit :: rest

            else if isTriplet candidate then
                let
                    rest =
                        findGroupsInSuit xs suit
                in
                if List.isEmpty rest && not emptyRemaining then
                    []

                else
                    Group Triplet x suit :: rest

            else
                []

        -- we only search for pairs at the end of the list
        x :: [ y ] ->
            if x == y then
                [ Group Pair x suit ]

            else
                []

        _ ->
            []



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
        perms =
            permutations tileNumbers

        -- TODO remove permutations of 3, abc def == def abc
        sortedPerms =
            List.sort perms
    in
    deduplicate sortedPerms


findGroups : List Tile -> GroupsPerSuit
findGroups tiles =
    let
        part =
            partitionBySuit tiles

        findAllGroups =
            \t withRuns ->
                List.map .number t
                    |> permutationsAndDedup
                    |> List.map (\p -> findGroupsInSuit p withRuns)
                    |> List.sortBy (\g -> List.map .tileNumber g)
                    |> deduplicate
                    |> List.filter (\g -> not (List.isEmpty g))

        groupsPerSuit =
            { sou = findAllGroups part.sou Sou
            , man = findAllGroups part.man Man
            , pin = findAllGroups part.pin Pin
            , honor = findAllGroups part.honor Honor
            }
    in
    groupsPerSuit


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


noFu : FuSource
noFu =
    FuSource 0 NoFu []


fuBase : Hand -> FuSource
fuBase _ =
    -- TODO seven pairs, 25 fu
    FuSource 20 BaseFu []


fuClosedRon : Hand -> FuSource
fuClosedRon hand =
    -- TODO only for *closed* ron
    if hand.winBy == Ron then
        FuSource 10 ClosedRon []

    else
        noFu


fuValuePair : Hand -> FuSource
fuValuePair hand =
    let
        possiblePair =
            getPair hand
    in
    case possiblePair of
        Just pair ->
            let
                possibleWind =
                    groupToWind pair

                isRoundWind =
                    Just hand.roundWind == possibleWind

                isSeatWind =
                    Just hand.seatWind == possibleWind

                n =
                    pair.tileNumber
            in
            -- dragon
            if (n == 5 || n == 6 || n == 7) && pair.suit == Honor then
                FuSource 2 (ValuePair ByDragon) [ pair ]

            else if isRoundWind && isSeatWind then
                FuSource 4 (ValuePair BySeatAndRoundWind) [ pair ]

            else if isRoundWind then
                FuSource 2 (ValuePair ByRoundWind) [ pair ]

            else if isSeatWind then
                FuSource 2 (ValuePair BySeatWind) [ pair ]

            else
                noFu

        Nothing ->
            noFu


fuWaitType : Hand -> FuSource
fuWaitType hand =
    let
        waitType =
            waitTypeHand hand
    in
    case waitType of
        EdgeWait ->
            FuSource 2 (WaitFu EdgeWait) []

        ClosedWait ->
            FuSource 2 (WaitFu ClosedWait) []

        PairWait ->
            FuSource 2 (WaitFu PairWait) []

        OpenWait ->
            noFu

        NoWait ->
            noFu


fuTriplet : Group -> FuSource
fuTriplet group =
    if group.type_ == Triplet then
        -- TODO closed
        if group.tileNumber == 1 || group.tileNumber == 9 then
            FuSource 8 (TripletFu Closed IsTerminal) [ group ]

        else if group.suit == Honor then
            FuSource 8 (TripletFu Closed IsHonor) [ group ]

        else
            FuSource 4 (TripletFu Closed HasNoValue) [ group ]

    else
        noFu


fuTriplets : Hand -> List FuSource
fuTriplets hand =
    let
        triplets =
            List.filter (\g -> g.type_ == Triplet) hand.groups
    in
    List.map fuTriplet triplets


countFu : Hand -> Hand
countFu hand =
    let
        base =
            fuBase hand

        closedRon =
            fuClosedRon hand

        valuePair =
            fuValuePair hand

        waitFu =
            fuWaitType hand

        triplets =
            fuTriplets hand

        allFu =
            List.concat [ [ base, closedRon, valuePair, waitFu ], triplets ]

        allValidFu =
            List.filter (\f -> not (f == noFu)) allFu
    in
    { hand | fu = allValidFu }


renderFuDetails : List FuSource -> Html Msg
renderFuDetails fuSources =
    table [] <|
        List.concat [ List.map renderFuSource fuSources, renderTotalFu fuSources ]


renderFuSource : FuSource -> Html Msg
renderFuSource fuSource =
    let
        explanation =
            case fuSource.description of
                BaseFu ->
                    "Base hand value"

                TsumoNotPinfu ->
                    "Tsumo (if not pinfu)"

                ClosedRon ->
                    "Closed ron"

                ValuePair ByDragon ->
                    "Value pair (dragon)"

                ValuePair ByRoundWind ->
                    "Value pair (round wind)"

                ValuePair BySeatWind ->
                    "Value pair (seat wind)"

                ValuePair BySeatAndRoundWind ->
                    "Value pair (seat & round wind)"

                WaitFu waitType ->
                    waitTypeToString waitType

                TripletFu openClosed kind ->
                    let
                        openClosedStr =
                            case openClosed of
                                Open ->
                                    "(open)"

                                Closed ->
                                    "(closed)"
                    in
                    case kind of
                        IsHonor ->
                            "Triplet of honors " ++ openClosedStr

                        IsTerminal ->
                            "Triplet of terminals " ++ openClosedStr

                        HasNoValue ->
                            "Triplet of simples " ++ openClosedStr

                -- TODO
                KanFu _ _ ->
                    "Kan"

                NoFu ->
                    "?"
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
        [ p [ onClick ChangeWinBy ] [ text <| "Win by: " ++ winByToString hand.winBy ] ]


renderWinds : Hand -> Html Msg
renderWinds hand =
    div []
        [ p [ onClick ChangeSeatWind ] [ text ("Seat wind: " ++ windToString hand.seatWind) ]
        , p [ onClick ChangeRoundWind ] [ text ("Round wind: " ++ windToString hand.roundWind) ]
        ]


cycleWind : Wind -> Wind
cycleWind wind =
    case wind of
        East ->
            South

        South ->
            West

        West ->
            North

        North ->
            East


winByToString : WinBy -> String
winByToString winBy =
    case winBy of
        Tsumo ->
            "Tsumo"

        Ron ->
            "Ron"


groupToWind : Group -> Maybe Wind
groupToWind group =
    let
        getWind g =
            if g.suit == Honor then
                case group.tileNumber of
                    1 ->
                        Just East

                    2 ->
                        Just South

                    3 ->
                        Just West

                    4 ->
                        Just North

                    _ ->
                        Nothing

            else
                Nothing
    in
    case group.type_ of
        Triplet ->
            getWind group

        Pair ->
            getWind group

        Run ->
            Nothing


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


groupToString : Group -> String
groupToString group =
    case group.type_ of
        Triplet ->
            String.repeat 3 (String.fromInt group.tileNumber) ++ suitToString group.suit

        Pair ->
            String.repeat 2 (String.fromInt group.tileNumber) ++ suitToString group.suit

        Run ->
            String.join ""
                [ String.fromInt group.tileNumber
                , String.fromInt (group.tileNumber + 1)
                , String.fromInt (group.tileNumber + 2)
                , suitToString group.suit
                ]


windToString : Wind -> String
windToString wind =
    case wind of
        East ->
            "East"

        South ->
            "South"

        West ->
            "West"

        North ->
            "North"


isChiitoitsu : Hand -> Yaku
isChiitoitsu { tiles } =
    let
        groups =
            findAllPairs tiles |> deduplicate
    in
    if List.length groups == 7 then
        Chiitoitsu

    else
        NoYaku


findAllPairs : List Tile -> List Group
findAllPairs tiles =
    case tiles of
        [] ->
            []

        [ _ ] ->
            []

        x :: y :: xs ->
            if x == y then
                Group Pair x.number x.suit :: findAllPairs xs

            else
                []


noYaku : HanSource
noYaku =
    HanSource 0 NoYaku


checkTanyao : Hand -> HanSource
checkTanyao hand =
    let
        isSimple group =
            if group.suit == Honor then
                False

            else
                case ( group.type_, group.tileNumber ) of
                    ( Triplet, 1 ) ->
                        False

                    ( Triplet, 9 ) ->
                        False

                    ( Run, 1 ) ->
                        False

                    ( Run, 7 ) ->
                        False

                    ( Pair, _ ) ->
                        True

                    ( Triplet, _ ) ->
                        True

                    ( Run, _ ) ->
                        True
    in
    if List.all isSimple hand.groups then
        HanSource 1 Tanyao

    else
        noYaku


groupIsTriplet : Group -> Bool
groupIsTriplet group =
    group.type_ == Triplet


groupIsPair : Group -> Bool
groupIsPair group =
    group.type_ == Pair


groupIsRun : Group -> Bool
groupIsRun group =
    group.type_ == Run


isDragon : Group -> Bool
isDragon group =
    groupIsTriplet group && group.suit == Honor && List.member group.tileNumber [ 5, 6, 7 ]


containsTerminal : Group -> Bool
containsTerminal group =
    if group.suit == Honor then
        False

    else
        case group.type_ of
            Triplet ->
                group.tileNumber == 1 || group.tileNumber == 9

            Pair ->
                group.tileNumber == 1 || group.tileNumber == 9

            Run ->
                group.tileNumber == 1 || group.tileNumber == 7


checkToitoi : Hand -> HanSource
checkToitoi hand =
    let
        -- TODO kan
        triplets =
            List.filter groupIsTriplet hand.groups
    in
    if List.length triplets == 4 then
        HanSource 2 Toitoi

    else
        noYaku


checkYakuhai : Hand -> List HanSource
checkYakuhai hand =
    let
        triplets =
            List.filter (\g -> groupIsTriplet g && isDragon g) hand.groups
    in
    if not (List.isEmpty triplets) then
        List.map (\_ -> HanSource 1 Yakuhai) triplets

    else
        []


handIsClosed : Hand -> Bool
handIsClosed _ =
    -- TODO open
    True


incrementHanIfClosed : Hand -> HanSource -> HanSource
incrementHanIfClosed hand hanSource =
    if handIsClosed hand then
        { hanSource | han = hanSource.han + 1 }

    else
        hanSource


checkChanta : Hand -> HanSource
checkChanta hand =
    let
        containsTerminalOrHonor g =
            g.suit == Honor || containsTerminal g
    in
    if List.all containsTerminalOrHonor hand.groups then
        HanSource 1 Chanta |> incrementHanIfClosed hand

    else
        noYaku


checkSanshokuDoujun : Hand -> HanSource
checkSanshokuDoujun hand =
    let
        sameSequence n =
            List.member (Group Run n Man) hand.groups
                && List.member (Group Run n Pin) hand.groups
                && List.member (Group Run n Sou) hand.groups

        checkRes =
            List.range 1 7
                |> List.map sameSequence
    in
    if List.any identity checkRes then
        HanSource 1 SanshokuDoujun |> incrementHanIfClosed hand

    else
        noYaku


checkSanshokuDoukou : Hand -> HanSource
checkSanshokuDoukou hand =
    let
        sameTriplet n =
            List.member (Group Triplet n Man) hand.groups
                && List.member (Group Triplet n Pin) hand.groups
                && List.member (Group Triplet n Sou) hand.groups

        checkRes =
            List.range 1 9
                |> List.map sameTriplet
    in
    if List.any identity checkRes then
        HanSource 2 SanshokuDoukou

    else
        noYaku


checkIipeikou : Hand -> HanSource
checkIipeikou hand =
    if handIsClosed hand then
        let
            runs =
                List.filter groupIsRun hand.groups
                    |> List.sortBy (\g -> ( suitToString g.suit, g.tileNumber ))

            res =
                List.map2 (\g1 g2 -> g1 == g2) runs (List.tail runs |> Maybe.withDefault [])
                    |> List.filter identity
        in
        if List.length res == 2 then
            HanSource 13 Ryanpeikou

        else if List.length res == 1 then
            HanSource 1 Iipeikou

        else
            noYaku

    else
        noYaku


getPair : Hand -> Maybe Group
getPair hand =
    let
        pairs =
            List.filter groupIsPair hand.groups
    in
    case pairs of
        [ x ] ->
            Just x

        _ ->
            Nothing


checkPinfu : Hand -> HanSource
checkPinfu hand =
    if handIsClosed hand then
        let
            runs =
                List.filter groupIsRun hand.groups

            pairIsValueLessPair =
                fuValuePair hand == noFu

            isTwoSidedOpenWait =
                waitTypeHand hand == OpenWait
        in
        if List.length runs == 4 && pairIsValueLessPair && isTwoSidedOpenWait then
            HanSource 1 Pinfu

        else
            noYaku

    else
        noYaku


winningTile : Hand -> Maybe Tile
winningTile hand =
    List.reverse hand.tiles
        |> List.head


waitTypeHand : Hand -> WaitType
waitTypeHand hand =
    let
        winTile =
            winningTile hand
    in
    case winTile of
        Just t ->
            let
                waits =
                    List.map (waitTypeGroup t) hand.groups
                        |> List.filter (\w -> w /= NoWait)
            in
            if List.member PairWait waits then
                PairWait

            else
                --- TODO pick one with better scoring
                List.head waits
                    |> Maybe.withDefault NoWait

        Nothing ->
            NoWait


waitTypeGroup : Tile -> Group -> WaitType
waitTypeGroup tile group =
    if tile.suit == group.suit then
        case group.type_ of
            Pair ->
                if tile.number == group.tileNumber then
                    PairWait

                else
                    NoWait

            Triplet ->
                NoWait

            Run ->
                if group.tileNumber == 1 && tile.number == 3 then
                    EdgeWait

                else if group.tileNumber == 7 && tile.number == 7 then
                    EdgeWait

                else if group.tileNumber + 1 == tile.number then
                    ClosedWait

                else if group.tileNumber == tile.number then
                    OpenWait

                else if group.tileNumber + 2 == tile.number then
                    OpenWait

                else
                    NoWait

    else
        NoWait


waitTypeToString : WaitType -> String
waitTypeToString waitType =
    case waitType of
        EdgeWait ->
            "Edge wait"

        ClosedWait ->
            "Closed wait"

        OpenWait ->
            "Open wait"

        PairWait ->
            "Pair wait"

        NoWait ->
            "-"
