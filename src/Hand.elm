module Hand exposing
    ( FuDescription(..)
    , FuSource
    , HanSource
    , Hand
    , WinBy(..)
    , Yaku(..)
    , checkAllYaku
    , countFu
    , fuDescriptionToString
    , hanDescriptionToString
    , init
    , setHanSources
    , winByToString
    )

import Tile
    exposing
        ( Group
        , GroupType(..)
        , Tile
        , Wind
        , containsTerminal
        , greenDragonNumber
        , groupIsPair
        , groupIsRun
        , groupIsTriplet
        , groupToWind
        , isDragon
        , redDragonNumber
        , suitToString
        , whiteDragonNumber
        )


type alias Hand =
    { tiles : List Tile
    , groups : List Group
    , winBy : WinBy
    , seatWind : Wind
    , roundWind : Wind
    , han : List HanSource
    , hanCount : Int
    , fu : List FuSource
    }


type WinBy
    = Ron
    | Tsumo


type alias FuSource =
    { fu : Int
    , description : FuDescription
    , groups : List Group
    }


type alias HanSource =
    { han : Int
    , description : Yaku
    }


type FuDescription
    = BaseFu
    | TsumoNotPinfu
    | ClosedRon
    | ValuePair ValuePairBy
    | WaitFu WaitType
    | TripletFu OpenClose TripletKind
    | KanFu OpenClose TripletKind
    | NoFu


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


type WaitType
    = OpenWait
    | ClosedWait
    | EdgeWait
    | PairWait
    | NoWait


type Yaku
    = Chiitoitsu
    | Pinfu
    | Iipeikou
    | Ryanpeikou
    | Yakuhai
    | Shousangen
    | Tanyao
    | SanshokuDoujun
    | Chanta
    | Toitoi
    | SanshokuDoukou


init : Hand
init =
    Hand [] [] Tsumo Tile.East Tile.East [] 0 []


winByToString : WinBy -> String
winByToString winBy =
    case winBy of
        Tsumo ->
            "Tsumo"

        Ron ->
            "Ron"


fuBase : Hand -> FuSource
fuBase _ =
    -- TODO seven pairs, 25 fu
    FuSource 20 BaseFu []


fuTsumoNotPinfu : Hand -> Maybe FuSource
fuTsumoNotPinfu hand =
    let
        isPinfu =
            List.any (\h -> h.description == Pinfu) hand.han
    in
    if hand.winBy == Tsumo && not isPinfu then
        Just (FuSource 2 TsumoNotPinfu [])

    else
        Nothing


fuClosedRon : Hand -> Maybe FuSource
fuClosedRon hand =
    if hand.winBy == Ron && handIsClosed hand then
        Just (FuSource 10 ClosedRon [])

    else
        Nothing


fuValuePair : Hand -> Maybe FuSource
fuValuePair hand =
    let
        determineFu pair =
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
            if (n == whiteDragonNumber || n == greenDragonNumber || n == redDragonNumber) && pair.suit == Tile.Honor then
                Just (FuSource 2 (ValuePair ByDragon) [ pair ])

            else if isRoundWind && isSeatWind then
                Just (FuSource 4 (ValuePair BySeatAndRoundWind) [ pair ])

            else if isRoundWind then
                Just (FuSource 2 (ValuePair ByRoundWind) [ pair ])

            else if isSeatWind then
                Just (FuSource 2 (ValuePair BySeatWind) [ pair ])

            else
                Nothing
    in
    getPair hand
        |> Maybe.andThen determineFu


fuWaitType : Hand -> Maybe FuSource
fuWaitType hand =
    let
        determineFu res =
            case res of
                ( EdgeWait, group ) ->
                    Just (FuSource 2 (WaitFu EdgeWait) [ group ])

                ( ClosedWait, group ) ->
                    Just (FuSource 2 (WaitFu ClosedWait) [ group ])

                ( PairWait, group ) ->
                    Just (FuSource 2 (WaitFu PairWait) [ group ])

                ( OpenWait, _ ) ->
                    Nothing

                ( NoWait, _ ) ->
                    Nothing
    in
    waitTypeHand hand
        |> Maybe.andThen determineFu


fuTriplet : Group -> Maybe FuSource
fuTriplet group =
    if group.type_ == Triplet then
        -- TODO closed
        if group.tileNumber == 1 || group.tileNumber == 9 then
            Just (FuSource 8 (TripletFu Closed IsTerminal) [ group ])

        else if group.suit == Tile.Honor then
            Just (FuSource 8 (TripletFu Closed IsHonor) [ group ])

        else
            Just (FuSource 4 (TripletFu Closed HasNoValue) [ group ])

    else
        Nothing


fuTriplets : Hand -> List (Maybe FuSource)
fuTriplets hand =
    let
        triplets =
            List.filter (\g -> g.type_ == Triplet) hand.groups
    in
    List.map fuTriplet triplets


hanDescriptionToString : Yaku -> String
hanDescriptionToString hanSource =
    case hanSource of
        Chiitoitsu ->
            "Chiitoitsu"

        Pinfu ->
            "Pinfu"

        Iipeikou ->
            "Iipeikou"

        Ryanpeikou ->
            "Ryanpeikou"

        Yakuhai ->
            "Yakuhai"

        Shousangen ->
            "Shousangen"

        Tanyao ->
            "Tanyao"

        SanshokuDoujun ->
            "SanshokuDoujun"

        Chanta ->
            "Chanta"

        Toitoi ->
            "Toitoi"

        SanshokuDoukou ->
            "SanshokuDoukou"


countFu : Hand -> Hand
countFu hand =
    let
        base =
            fuBase hand

        tsumoNotPinfu =
            fuTsumoNotPinfu hand

        closedRon =
            fuClosedRon hand

        valuePair =
            fuValuePair hand

        waitFu =
            fuWaitType hand

        triplets =
            fuTriplets hand

        allFu =
            List.concat [ [ Just base, tsumoNotPinfu, closedRon, valuePair, waitFu ], triplets ]

        allValidFu =
            List.filterMap identity allFu
    in
    { hand | fu = allValidFu }


fuDescriptionToString : FuDescription -> String
fuDescriptionToString fuDescription =
    case fuDescription of
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


checkTanyao : Hand -> Maybe HanSource
checkTanyao hand =
    let
        isSimple group =
            if group.suit == Tile.Honor then
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
        Just (HanSource 1 Tanyao)

    else
        Nothing


checkToitoi : Hand -> Maybe HanSource
checkToitoi hand =
    let
        -- TODO kan
        triplets =
            List.filter groupIsTriplet hand.groups
    in
    if List.length triplets == 4 then
        Just (HanSource 2 Toitoi)

    else
        Nothing


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


checkChanta : Hand -> Maybe HanSource
checkChanta hand =
    let
        containsTerminalOrHonor g =
            g.suit == Tile.Honor || containsTerminal g
    in
    if List.all containsTerminalOrHonor hand.groups then
        HanSource 1 Chanta
            |> incrementHanIfClosed hand
            |> Just

    else
        Nothing


checkSanshokuDoujun : Hand -> Maybe HanSource
checkSanshokuDoujun hand =
    let
        sameSequence n =
            List.member (Group Run n Tile.Man) hand.groups
                && List.member (Group Run n Tile.Pin) hand.groups
                && List.member (Group Run n Tile.Sou) hand.groups

        checkRes =
            List.range 1 7
                |> List.map sameSequence
    in
    if List.any identity checkRes then
        HanSource 1 SanshokuDoujun
            |> incrementHanIfClosed hand
            |> Just

    else
        Nothing


checkSanshokuDoukou : Hand -> Maybe HanSource
checkSanshokuDoukou hand =
    let
        sameTriplet n =
            List.member (Group Triplet n Tile.Man) hand.groups
                && List.member (Group Triplet n Tile.Pin) hand.groups
                && List.member (Group Triplet n Tile.Sou) hand.groups

        checkRes =
            List.range 1 9
                |> List.map sameTriplet
    in
    if List.any identity checkRes then
        Just (HanSource 2 SanshokuDoukou)

    else
        Nothing


checkIipeikou : Hand -> Maybe HanSource
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
            Just (HanSource 13 Ryanpeikou)

        else if List.length res == 1 then
            Just (HanSource 1 Iipeikou)

        else
            Nothing

    else
        Nothing


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


checkPinfu : Hand -> Maybe HanSource
checkPinfu hand =
    if handIsClosed hand then
        let
            runs =
                List.filter groupIsRun hand.groups

            pairIsValueLessPair =
                fuValuePair hand == Nothing

            waitType =
                waitTypeHand hand
                    |> Maybe.map Tuple.first
        in
        if List.length runs == 4 && pairIsValueLessPair && waitType == Just OpenWait then
            Just (HanSource 1 Pinfu)

        else
            Nothing

    else
        Nothing


checkShousangen : Hand -> Maybe HanSource
checkShousangen hand =
    let
        pair =
            getPair hand

        isDragonPair =
            Maybe.map isDragon pair
                |> Maybe.withDefault False

        allDragons =
            [ Tile.whiteDragonNumber, Tile.greenDragonNumber, Tile.redDragonNumber ]
    in
    case ( pair, isDragonPair ) of
        ( Just dragonPair, True ) ->
            let
                remainingDragons =
                    List.filter (\d -> d /= dragonPair.tileNumber) allDragons

                findTriplet tileNumber =
                    List.filter (\n -> n == Group Triplet tileNumber Tile.Honor) hand.groups

                groups =
                    List.concatMap (\d -> findTriplet d) remainingDragons
            in
            if List.length groups == 2 then
                Just (HanSource 2 Shousangen)

            else
                Nothing

        _ ->
            Nothing


winningTile : Hand -> Maybe Tile
winningTile hand =
    List.reverse hand.tiles
        |> List.head


waitTypeHand : Hand -> Maybe ( WaitType, Group )
waitTypeHand hand =
    let
        winTile =
            winningTile hand
    in
    case winTile of
        Just t ->
            let
                waits =
                    List.map (\g -> Tuple.pair (waitTypeGroup t g) g) hand.groups
                        |> List.filter (\tt -> Tuple.first tt /= NoWait)

                pairWait =
                    List.filter (\tt -> Tuple.first tt == PairWait) waits
                        |> List.head
            in
            case pairWait of
                Just ( PairWait, group ) ->
                    Just ( PairWait, group )

                _ ->
                    --- TODO pick one with better scoring
                    List.head waits

        _ ->
            Nothing


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


yakuChecks : List (Hand -> Maybe HanSource)
yakuChecks =
    [ checkIipeikou
    , checkShousangen
    , checkTanyao
    , checkToitoi
    , checkChanta
    , checkSanshokuDoujun
    , checkSanshokuDoukou
    , checkPinfu
    ]


checkAllYaku : Hand -> List HanSource
checkAllYaku hand =
    let
        checks =
            List.map (\c -> c hand) yakuChecks
    in
    List.filterMap identity checks
        |> List.append (checkYakuhai hand)


setHanSources : List HanSource -> Hand -> Hand
setHanSources hanSources hand =
    let
        totalHan =
            List.map .han hanSources
                |> List.sum
    in
    { hand | han = hanSources, hanCount = totalHan }
