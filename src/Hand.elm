module Hand exposing
    ( FuDescription(..)
    , FuSource
    , HanSource
    , Hand
    , WinBy(..)
    , Yaku(..)
    , count
    , fuDescriptionToString
    , getHandString
    , hanDescriptionToString
    , init
    , isDealer
    , isRon
    , isTsumo
    , isWinningHand
    , randomTenpaiHand
    , randomWinningHand
    , score
    , scoreCell
    , setHanSources
    , shouldCountFu
    , winByToString
    , winningTiles
    )

import Array
import Group exposing (Group, GroupType(..))
import Random
import Score
import Set
import Tile
    exposing
        ( Tile
        , Wind
        , greenDragonNumber
        , redDragonNumber
        , whiteDragonNumber
        )


type alias Hand =
    { tiles : List Tile
    , groups : List Group
    , winBy : WinBy
    , seatWind : Wind
    , roundWind : Wind
    , hanSources : List HanSource
    , hanCount : Int
    , fuSources : List FuSource
    , fuCount : Int
    , fuCountBeforeRounding : Int
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
    | MenzenTsumo
    | Pinfu
    | Iipeikou
    | Ryanpeikou
    | Yakuhai
    | Shousangen
    | Daisangen
    | Shousuushi
    | Daisuushi
    | Tanyao
    | SanshokuDoujun
    | Ittsu
    | Chanta
    | Junchan
    | Honroutou
    | Chinroutou
    | Tsuuiisou
    | Toitoi
    | Sanankou
    | Suuankou
    | SanshokuDoukou
    | Honitsu
    | Chinitsu


init : Hand
init =
    Hand [] [] Tsumo Tile.East Tile.East [] 0 [] 0 0


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
            List.any (\h -> h.description == Pinfu) hand.hanSources
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
                    Group.toWind pair

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

        MenzenTsumo ->
            "Menzen Tsumo"

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

        Daisangen ->
            "Daisangen"

        Shousuushi ->
            "Shousuushi"

        Daisuushi ->
            "Daisuushi"

        Tanyao ->
            "Tanyao"

        SanshokuDoujun ->
            "Sanshoku Doujun"

        Ittsu ->
            "Ittsu"

        Chanta ->
            "Chanta"

        Junchan ->
            "Junchan"

        Honroutou ->
            "Honroutou"

        Chinroutou ->
            "Chinroutou"

        Tsuuiisou ->
            "Tsuuiisou"

        Toitoi ->
            "Toitoi"

        Sanankou ->
            "Sanankou"

        Suuankou ->
            "Suuankou"

        SanshokuDoukou ->
            "Sanshoku Doukou"

        Honitsu ->
            "Honitsu"

        Chinitsu ->
            "Chinitsu"


countFu : Hand -> Hand
countFu hand =
    if shouldCountFu hand then
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

            sumFu =
                List.map .fu allValidFu
                    |> List.sum

            roundedFu =
                toFloat sumFu
                    / 10
                    |> ceiling
                    |> (*) 10
        in
        { hand | fuSources = allValidFu, fuCount = roundedFu, fuCountBeforeRounding = sumFu }

    else
        hand


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


checkMenzenTsumo : Hand -> Maybe HanSource
checkMenzenTsumo hand =
    if hand.winBy == Tsumo && handIsClosed hand then
        Just (HanSource 1 MenzenTsumo)

    else
        Nothing


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
            List.filter Group.isTriplet hand.groups
    in
    if List.length triplets == 4 then
        Just (HanSource 2 Toitoi)

    else
        Nothing


checkYakuhai : Hand -> List HanSource
checkYakuhai hand =
    let
        isRoundWind g =
            g == Group Triplet (Tile.windToTileNumber hand.roundWind) Tile.Honor

        isSeatWind g =
            g == Group Triplet (Tile.windToTileNumber hand.seatWind) Tile.Honor

        triplets =
            List.filter (\g -> Group.isTriplet g && (Group.isDragon g || isRoundWind g || isSeatWind g)) hand.groups
    in
    if not (List.isEmpty triplets) then
        List.map
            (\g ->
                if isRoundWind g && isSeatWind g then
                    HanSource 2 Yakuhai

                else
                    HanSource 1 Yakuhai
            )
            triplets

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
            g.suit == Tile.Honor || Group.containsTerminal g

        numHonorGroups =
            List.filter Group.isHonor hand.groups
                |> List.length

        triplets =
            List.filter Group.isTriplet hand.groups

        allTriplets =
            List.length triplets == 4
    in
    if List.all containsTerminalOrHonor hand.groups then
        if numHonorGroups == 0 then
            if allTriplets then
                Just (HanSource 13 Chinroutou)

            else
                HanSource 2 Junchan
                    |> incrementHanIfClosed hand
                    |> Just

        else if allTriplets then
            Just (HanSource 2 Honroutou)

        else
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
                List.filter Group.isRun hand.groups
                    |> List.sortBy (\g -> ( Tile.suitToString g.suit, g.tileNumber ))

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
            List.filter Group.isPair hand.groups
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
                List.filter Group.isRun hand.groups

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
            Maybe.map Group.isDragon pair
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


checkDaisangen : Hand -> Maybe HanSource
checkDaisangen { groups } =
    if
        List.member (Group Triplet whiteDragonNumber Tile.Honor) groups
            && List.member (Group Triplet greenDragonNumber Tile.Honor) groups
            && List.member (Group Triplet redDragonNumber Tile.Honor) groups
    then
        Just (HanSource 13 Daisangen)

    else
        Nothing


checkSousuushi : Hand -> Maybe HanSource
checkSousuushi { groups } =
    let
        isWindTripletOrPair group =
            group.suit
                == Tile.Honor
                && (group.type_ == Group.Triplet || group.type_ == Group.Pair)
                && group.tileNumber
                <= 4

        windGroups =
            List.filter isWindTripletOrPair groups

        windGroupsNumbers =
            List.map .tileNumber windGroups

        pairs =
            List.filter Group.isPair windGroups
    in
    if
        List.length windGroups
            == 4
            && List.length pairs
            == 1
            && List.member 1 windGroupsNumbers
            && List.member 2 windGroupsNumbers
            && List.member 3 windGroupsNumbers
            && List.member 4 windGroupsNumbers
    then
        Just (HanSource 13 Shousuushi)

    else
        Nothing


checkDaisuushi : Hand -> Maybe HanSource
checkDaisuushi { groups } =
    if
        List.member (Group Triplet 1 Tile.Honor) groups
            && List.member (Group Triplet 2 Tile.Honor) groups
            && List.member (Group Triplet 3 Tile.Honor) groups
            && List.member (Group Triplet 4 Tile.Honor) groups
    then
        -- TODO double yakuman
        Just (HanSource 13 Daisuushi)

    else
        Nothing


checkIttsu : Hand -> Maybe HanSource
checkIttsu hand =
    let
        hasIttsu suit =
            List.member (Group Run 1 suit) hand.groups
                && List.member (Group Run 4 suit) hand.groups
                && List.member (Group Run 7 suit) hand.groups
    in
    if hasIttsu Tile.Man || hasIttsu Tile.Pin || hasIttsu Tile.Sou then
        Just (HanSource 1 Ittsu |> incrementHanIfClosed hand)

    else
        Nothing


checkHonitsuTsuuiisou : Hand -> Maybe HanSource
checkHonitsuTsuuiisou hand =
    let
        suits =
            List.map .suit hand.groups
                |> List.map Tile.suitToString
                |> Set.fromList

        honor =
            Tile.suitToString Tile.Honor
    in
    if Set.size suits == 1 && Set.member honor suits then
        Just (HanSource 13 Tsuuiisou)

    else if Set.size suits == 1 && not (Set.member honor suits) then
        Just (HanSource 5 Chinitsu |> incrementHanIfClosed hand)

    else if Set.size suits == 2 && Set.member honor suits then
        Just (HanSource 2 Honitsu |> incrementHanIfClosed hand)

    else
        Nothing


checkSanankouAndSuuankou : Hand -> Maybe HanSource
checkSanankouAndSuuankou hand =
    let
        -- TODO filter out the group with the ron tile
        triplets =
            List.filter (\g -> Group.isTriplet g && Group.isClosed g) hand.groups

        countTriplets =
            List.length triplets
    in
    if countTriplets == 4 then
        Just (HanSource 13 Suuankou)

    else if countTriplets == 3 then
        Just (HanSource 2 Sanankou)

    else
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
    [ checkMenzenTsumo
    , checkIipeikou
    , checkShousangen
    , checkDaisangen
    , checkSousuushi
    , checkDaisuushi
    , checkTanyao
    , checkToitoi
    , checkSanankouAndSuuankou
    , checkChanta
    , checkSanshokuDoujun
    , checkIttsu
    , checkSanshokuDoukou
    , checkPinfu
    , checkHonitsuTsuuiisou
    ]


checkAllYaku : Hand -> List HanSource
checkAllYaku hand =
    let
        checks =
            List.map (\c -> c hand) yakuChecks
    in
    List.filterMap identity checks
        |> List.append (checkYakuhai hand)
        |> keepYakumanIfPresent


keepYakumanIfPresent : List HanSource -> List HanSource
keepYakumanIfPresent hanSources =
    let
        yakuman =
            List.filter (\h -> h.han >= 13) hanSources
    in
    if List.isEmpty yakuman then
        hanSources

    else
        yakuman


setHanSources : List HanSource -> Hand -> Hand
setHanSources hanSources hand =
    let
        totalHan =
            List.map .han hanSources
                |> List.sum
    in
    { hand | hanSources = hanSources, hanCount = totalHan }


count : Hand -> Hand
count hand =
    let
        allYaku =
            checkAllYaku hand
    in
    setHanSources allYaku hand
        |> countFu


randomSuit : Random.Generator Tile.Suit
randomSuit =
    Random.uniform Tile.Man [ Tile.Pin, Tile.Sou, Tile.Honor ]


randomWind : Random.Generator Wind
randomWind =
    Random.uniform Tile.East [ Tile.South, Tile.West, Tile.North ]


randomWinBy : Random.Generator WinBy
randomWinBy =
    Random.uniform Ron [ Tsumo ]


randomTripletOrRun : Random.Generator Group
randomTripletOrRun =
    let
        maxRange suit =
            if suit == Tile.Honor then
                7

            else
                9 + 7
    in
    randomSuit
        |> Random.andThen (\s -> Random.pair (Random.constant s) (Random.int 1 (maxRange s)))
        |> Random.map
            (\( suit, n ) ->
                if n < 10 then
                    Group Triplet n suit

                else
                    Group Run (n - 9) suit
            )


randomPair : Random.Generator Group
randomPair =
    let
        maxRange suit =
            if suit == Tile.Honor then
                7

            else
                9
    in
    Random.uniform Tile.Man [ Tile.Pin, Tile.Sou, Tile.Honor ]
        |> Random.andThen (\s -> Random.pair (Random.int 1 (maxRange s)) (Random.constant s))
        |> Random.map (\( n, suit ) -> Group Pair n suit)


randomWinningGroups : Random.Generator (List Group)
randomWinningGroups =
    let
        groups =
            Random.list 4 randomTripletOrRun

        pair =
            randomPair
    in
    Random.map2 (\g p -> List.append g [ p ]) groups pair


randomWinningHand : Random.Generator Hand
randomWinningHand =
    let
        hand =
            init

        winningTilePosition =
            Random.int 0 13

        createHand groups winBy seatWind roundWind winTilePos =
            let
                tiles =
                    List.map Group.toTiles groups
                        |> List.concat
                        |> Array.fromList
                        |> Tile.moveWinningTileToEnd winTilePos
                        |> Array.toList
            in
            { hand | tiles = tiles, groups = groups, winBy = winBy, seatWind = seatWind, roundWind = roundWind }
    in
    Random.map5 createHand randomWinningGroups randomWinBy randomWind randomWind winningTilePosition
        |> Random.andThen
            (\h ->
                if Tile.hasMoreThan4Tiles h.tiles then
                    randomWinningHand

                else
                    Random.constant h
            )


randomTenpaiHand : Random.Generator Hand
randomTenpaiHand =
    let
        tileToRemovePosition =
            Random.int 0 13

        toTenpai hand pos =
            let
                tiles =
                    Array.fromList hand.tiles
                        |> Tile.removeTileAtPos pos
                        |> Array.toList
                        |> Tile.sort
            in
            { hand | tiles = tiles, groups = [] }
    in
    Random.map2 toTenpai randomWinningHand tileToRemovePosition


shouldCountFu : Hand -> Bool
shouldCountFu hand =
    hand.hanCount < 5


getHandString : Hand -> String
getHandString hand =
    if List.length hand.tiles == 13 then
        List.map Tile.toString hand.tiles |> String.join ""

    else
        List.map Group.toString hand.groups |> String.join ""


type alias DealerScore =
    { ron : Int
    , tsumo : Int
    }


type alias NonDealerScore =
    { ron : Int
    , tsumoDealer : Int
    , tsumoNonDealer : Int
    }


type alias Score =
    { dealer : DealerScore
    , nonDealer : NonDealerScore
    }


scoreCell : Hand -> Score
scoreCell { hanCount, fuCount } =
    Score.score hanCount fuCount


score : Hand -> Int
score hand =
    let
        cellScore =
            scoreCell hand
    in
    if playerIsDealer hand then
        case hand.winBy of
            Ron ->
                cellScore.dealer.ron

            Tsumo ->
                cellScore.dealer.tsumo

    else
        case hand.winBy of
            Ron ->
                cellScore.nonDealer.ron

            Tsumo ->
                if playerIsDealer hand then
                    cellScore.nonDealer.tsumoDealer

                else
                    cellScore.nonDealer.tsumoNonDealer


playerIsDealer : Hand -> Bool
playerIsDealer hand =
    hand.seatWind == Tile.East


isRon : Hand -> Bool
isRon { winBy } =
    winBy == Ron


isTsumo : Hand -> Bool
isTsumo { winBy } =
    winBy == Tsumo


isDealer : Hand -> Bool
isDealer { seatWind } =
    seatWind == Tile.East


winningTiles : Hand -> List Tile
winningTiles hand =
    if List.length hand.tiles == 13 then
        let
            generateHand ( tile, tiles ) =
                ( tile, count { hand | tiles = tiles, groups = Group.findWinningGroups (Group.findGroups tiles), hanCount = 0 } )
        in
        List.map (\t -> ( t, Tile.push t hand.tiles )) Tile.allTiles
            |> List.map generateHand
            |> List.filter (\( _, h ) -> isWinningHand h)
            |> List.map Tuple.first

    else
        []


isWinningHand : Hand -> Bool
isWinningHand hand =
    List.length hand.tiles == 14 && List.length hand.groups == 5 && hand.hanCount > 0 && not (Tile.hasMoreThan4Tiles hand.tiles)
