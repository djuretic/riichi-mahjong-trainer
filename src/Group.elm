module Group exposing
    ( Group
    , GroupType(..)
    , GroupsBreakdown
    , GroupsPerSuit
    , RandomSuitPreference(..)
    , commonGroups
    , containsTerminal
    , findGroups
    , findGroupsInSuit
    , findWinningGroups
    , isClosed
    , isDragon
    , isHonor
    , isPair
    , isRun
    , isTriplet
    , isTripletOf
    , isWinningHand
    , member
    , random4SidedWaitTwoSuits
    , random5SidedWait
    , randomCompleteGroups
    , randomTenpaiGroups
    , randomWinningGroups
    , toString
    , toTiles
    , toWind
    , winningTiles
    )

import Array
import Counter
import List.Extra
import Random
import Suit
import Tile


type GroupType
    = Triplet
    | Run
    | Pair


type alias Group =
    { type_ : GroupType

    -- for runs, is the first (lowest) tile
    , tileNumber : Tile.TileNumber
    , suit : Suit.Suit
    }


type alias GroupsPerSuit =
    { sou : List (List Group)
    , man : List (List Group)
    , pin : List (List Group)
    , honor : List (List Group)
    }


type alias GroupsBreakdown =
    { chiitoitsu : List Group
    , perSuit : GroupsPerSuit
    }


toWind : Group -> Maybe Tile.Wind
toWind group =
    let
        getWind g =
            if g.suit == Suit.Honor then
                case group.tileNumber of
                    1 ->
                        Just Tile.East

                    2 ->
                        Just Tile.South

                    3 ->
                        Just Tile.West

                    4 ->
                        Just Tile.North

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


type RandomSuitPreference
    = OneRandomSuit
    | OneSuitFixed Suit.Suit
    | TwoSuitsTwoFixed Suit.Suit Suit.Suit
    | TwoSuitsOneFixed Suit.Suit
    | TwoRandomSuits


findGroups : List Tile.Tile -> GroupsBreakdown
findGroups tiles =
    let
        part =
            Tile.partitionBySuit tiles
    in
    { chiitoitsu = findChiitoitsu (Tile.sort tiles)
    , perSuit =
        { sou = findGroupsInSuit Suit.Sou part.sou
        , man = findGroupsInSuit Suit.Man part.man
        , pin = findGroupsInSuit Suit.Pin part.pin
        , honor = findGroupsInSuit Suit.Honor part.honor
        }
    }


findChiitoitsu : List Tile.Tile -> List Group
findChiitoitsu tiles =
    let
        findPairsHelper : List Tile.Tile -> List Group -> List Group
        findPairsHelper remainingTiles acc =
            case remainingTiles of
                [] ->
                    acc

                [ _ ] ->
                    []

                x :: y :: xs ->
                    if x == y then
                        findPairsHelper xs (Group Pair x.number x.suit :: acc)

                    else
                        []

        pairs =
            findPairsHelper tiles [] |> List.reverse
    in
    if List.length pairs == 7 && List.length (Tile.deduplicate pairs) == 7 then
        pairs

    else
        []


findGroupsInSuit : Suit.Suit -> List Tile.Tile -> List (List Group)
findGroupsInSuit suit tiles =
    List.map .number tiles
        |> List.sort
        |> Tile.toArrayCounter
        |> findGroupsInSuitHelper suit 0 True
        |> Maybe.withDefault []


findGroupsInSuitHelper : Suit.Suit -> Int -> Bool -> Counter.Counter -> Maybe (List (List Group))
findGroupsInSuitHelper suit n shouldFindPair counter =
    let
        count =
            Array.get n counter
                |> Maybe.withDefault 0
    in
    if n >= Array.length counter then
        Just [ [] ]

    else if count == 0 then
        findGroupsInSuitHelper suit (n + 1) shouldFindPair counter

    else
        let
            foundTriplet =
                count >= 3

            triplet =
                if foundTriplet then
                    findGroupsInSuitHelper suit n shouldFindPair (Array.set n (count - 3) counter)
                        |> addGroupToHead (Group Triplet (n + 1) suit)

                else
                    Nothing

            foundPair =
                count >= 2

            pair =
                if foundPair && shouldFindPair then
                    findGroupsInSuitHelper suit n False (Array.set n (count - 2) counter)
                        |> addGroupToHead (Group Pair (n + 1) suit)

                else
                    Nothing

            foundRun =
                suit /= Suit.Honor && n < 7 && count >= 1 && Counter.getCount (n + 1) counter > 0 && Counter.getCount (n + 2) counter > 0

            run =
                if foundRun then
                    let
                        count2 =
                            Counter.getCount (n + 1) counter

                        count3 =
                            Counter.getCount (n + 2) counter

                        updatedCounter =
                            counter
                                |> Array.set n (count - 1)
                                |> Array.set (n + 1) (count2 - 1)
                                |> Array.set (n + 2) (count3 - 1)
                    in
                    findGroupsInSuitHelper suit n shouldFindPair updatedCounter
                        |> addGroupToHead (Group Run (n + 1) suit)

                else
                    Nothing
        in
        map2RetainJust List.append triplet run
            |> map2RetainJust List.append pair



-- using Maybe.map2 with only 1 Nothing just retains the Nothing


map2RetainJust : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
map2RetainJust func a b =
    case ( a, b ) of
        ( Nothing, bb ) ->
            bb

        ( aa, Nothing ) ->
            aa

        ( Just aa, Just bb ) ->
            Maybe.map2 func (Just aa) (Just bb)


addGroupToHead : Group -> Maybe (List (List Group)) -> Maybe (List (List Group))
addGroupToHead group foundGroups =
    Maybe.map (\fg -> List.map ((::) group) fg) foundGroups


toString : Group -> String
toString group =
    case group.type_ of
        Triplet ->
            String.repeat 3 (String.fromInt group.tileNumber) ++ Suit.toString group.suit

        Pair ->
            String.repeat 2 (String.fromInt group.tileNumber) ++ Suit.toString group.suit

        Run ->
            String.concat
                [ String.fromInt group.tileNumber
                , String.fromInt (group.tileNumber + 1)
                , String.fromInt (group.tileNumber + 2)
                , Suit.toString group.suit
                ]


isTriplet : Group -> Bool
isTriplet group =
    group.type_ == Triplet


isTripletOf : Tile.TileNumber -> Suit.Suit -> Group -> Bool
isTripletOf tileNumber suit group =
    isTriplet group && group.tileNumber == tileNumber && group.suit == suit


isPair : Group -> Bool
isPair group =
    group.type_ == Pair


isRun : Group -> Bool
isRun group =
    group.type_ == Run


isDragon : Group -> Bool
isDragon group =
    let
        isTripletOrPair =
            isTriplet group || isPair group
    in
    isTripletOrPair && group.suit == Suit.Honor && List.member group.tileNumber [ Tile.whiteDragonNumber, Tile.greenDragonNumber, Tile.redDragonNumber ]


isHonor : Group -> Bool
isHonor group =
    group.suit == Suit.Honor


containsTerminal : Group -> Bool
containsTerminal group =
    if group.suit == Suit.Honor then
        False

    else
        case group.type_ of
            Triplet ->
                group.tileNumber == 1 || group.tileNumber == 9

            Pair ->
                group.tileNumber == 1 || group.tileNumber == 9

            Run ->
                group.tileNumber == 1 || group.tileNumber == 7


toTiles : Group -> List Tile.Tile
toTiles { type_, tileNumber, suit } =
    case type_ of
        Pair ->
            List.repeat 2 (Tile.Tile tileNumber suit)

        Triplet ->
            List.repeat 3 (Tile.Tile tileNumber suit)

        Run ->
            [ Tile.Tile tileNumber suit
            , Tile.Tile (tileNumber + 1) suit
            , Tile.Tile (tileNumber + 2) suit
            ]


isClosed : Group -> Bool
isClosed _ =
    -- TODO
    True


findWinningGroups : GroupsBreakdown -> List Group
findWinningGroups groups =
    -- TODO consider chiitoitsu and other group configurations in the same hand
    if List.isEmpty groups.chiitoitsu then
        let
            firstItem =
                \g -> Maybe.withDefault [] (List.head g)

            man =
                firstItem groups.perSuit.man

            pin =
                firstItem groups.perSuit.pin

            sou =
                firstItem groups.perSuit.sou

            honor =
                firstItem groups.perSuit.honor

            possibleGroups =
                List.concat [ man, pin, sou, honor ]

            numberPairs =
                List.filter (\g -> g.type_ == Pair) possibleGroups |> List.length

            groupSort g =
                ( Suit.toString g.suit, g.tileNumber )
        in
        if numberPairs == 1 then
            List.sortBy groupSort possibleGroups

        else
            []

    else
        groups.chiitoitsu


commonGroups : List (List Group) -> List Group
commonGroups listGroups =
    case listGroups of
        [] ->
            []

        [ _ ] ->
            []

        x :: xs ->
            commonGroupsHelper x xs []


commonGroupsHelper : List Group -> List (List Group) -> List Group -> List Group
commonGroupsHelper baseGroups listGroups res =
    case baseGroups of
        [] ->
            res

        x :: xs ->
            let
                isPresent : Group -> List Group -> Bool
                isPresent elem list =
                    List.Extra.find (\e -> e == elem) list /= Nothing
            in
            if List.all identity (List.map (isPresent x) listGroups) then
                commonGroupsHelper xs (List.map (List.Extra.remove x) listGroups) (x :: res)

            else
                commonGroupsHelper xs listGroups res


randomPair : Random.Generator Group
randomPair =
    Random.uniform Suit.Man [ Suit.Pin, Suit.Sou, Suit.Honor ]
        |> Random.andThen (\s -> Random.pair (Random.int 1 (Suit.maxRange s)) (Random.constant s))
        |> Random.map (\( n, suit ) -> Group Pair n suit)


randomPairOf : Suit.Suit -> Random.Generator Group
randomPairOf suit =
    Random.int 1 (Suit.maxRange suit)
        |> Random.map (\n -> Group Pair n suit)


randomTripletOrRun : Random.Generator Group
randomTripletOrRun =
    let
        maxRange suit =
            if suit == Suit.Honor then
                7

            else
                9 + 7
    in
    Suit.randomSuit
        |> Random.andThen (\s -> Random.pair (Random.constant s) (Random.int 1 (maxRange s)))
        |> Random.map
            (\( suit, n ) ->
                if n < 10 then
                    Group Triplet n suit

                else
                    Group Run (n - 9) suit
            )


randomTripletOrRunOf : Int -> Suit.Suit -> Random.Generator Group
randomTripletOrRunOf tripletWeight suit =
    if suit == Suit.Honor then
        Random.int 1 7
            |> Random.map (\n -> Group Triplet n suit)

    else
        Random.weighted ( toFloat tripletWeight, Triplet ) [ ( toFloat (100 - tripletWeight), Run ) ]
            |> Random.andThen
                (\groupType ->
                    if groupType == Triplet then
                        randomTripletOf suit

                    else
                        randomRunOf suit
                )


randomTripletOf : Suit.Suit -> Random.Generator Group
randomTripletOf suit =
    Random.int 1 9
        |> Random.map (\n -> Group Triplet n suit)


randomRunOf : Suit.Suit -> Random.Generator Group
randomRunOf suit =
    Random.int 1 7
        |> Random.map (\n -> Group Run n suit)


randomWinningGroups : Random.Generator (List Group)
randomWinningGroups =
    let
        groups =
            Random.list 4 randomTripletOrRun

        pair =
            randomPair
    in
    Random.map2 (\g p -> List.append g [ p ]) groups pair


randomCompleteGroups : Int -> Int -> RandomSuitPreference -> Random.Generator (List Group)
randomCompleteGroups numNonPairs tripletWeight wantedSuit =
    let
        tripletGen suit =
            randomTripletOrRunOf 30 suit

        otherGroups suit =
            Random.list numNonPairs (tripletGen suit)

        otherGroupsTwoSuits suit1 suit2 =
            Random.int 0 (numNonPairs - 1)
                |> Random.andThen (\n -> Random.pair (Random.list n (tripletGen suit1)) (Random.list (numNonPairs - n) (tripletGen suit2)))
                |> Random.map (\( p, g ) -> List.append p g)

        baseGroups =
            case wantedSuit of
                OneSuitFixed s ->
                    Random.constant s
                        |> Random.andThen (\ss -> Random.pair (randomPairOf ss) (otherGroups ss))
                        |> Random.map (\( p, g ) -> p :: g)

                OneRandomSuit ->
                    Suit.randomNonHonorSuit
                        |> Random.andThen (\ss -> Random.pair (randomPairOf ss) (otherGroups ss))
                        |> Random.map (\( p, g ) -> p :: g)

                TwoSuitsTwoFixed s1 s2 ->
                    Random.uniform ( s1, s2 ) [ ( s2, s1 ) ]
                        |> Random.andThen (\( ss1, ss2 ) -> Random.pair (randomPairOf ss1) (otherGroupsTwoSuits ss1 ss2))
                        |> Random.map (\( p, g ) -> p :: g)

                TwoSuitsOneFixed s1 ->
                    Suit.randomSuitExcludingOne s1
                        |> Random.andThen (\s2 -> Random.uniform ( s1, s2 ) [ ( s2, s1 ) ])
                        |> Random.andThen (\( ss1, ss2 ) -> Random.pair (randomPairOf ss1) (otherGroupsTwoSuits ss1 ss2))
                        |> Random.map (\( p, g ) -> p :: g)

                TwoRandomSuits ->
                    Suit.randomTwoHonorSuits
                        |> Random.andThen (\( s1, s2 ) -> Random.pair (randomPairOf s1) (otherGroupsTwoSuits s1 s2))
                        |> Random.map (\( p, g ) -> p :: g)
    in
    baseGroups
        |> Random.andThen
            (\g ->
                let
                    tiles =
                        List.concatMap toTiles g
                in
                if Tile.hasMoreThan4Tiles tiles then
                    randomCompleteGroups numNonPairs tripletWeight wantedSuit

                else
                    Random.constant g
            )


randomTenpaiGroups : Int -> Int -> RandomSuitPreference -> Random.Generator (List Tile.Tile)
randomTenpaiGroups numNonPairs tripletWeight wantedSuit =
    let
        posToRemove =
            Random.int 0 (2 + (numNonPairs * 3) - 1)
    in
    randomCompleteGroups numNonPairs tripletWeight wantedSuit
        |> Random.map2
            (\pos lg ->
                List.concatMap toTiles lg
                    |> Tile.removeTileAtPosFromList pos
                    |> Tile.sort
            )
            posToRemove


random5SidedWait : RandomSuitPreference -> Random.Generator (List Tile.Tile)
random5SidedWait wantedSuit =
    Random.uniform True [ False ]
        |> Random.andThen
            (\b ->
                if b then
                    randomTatsumaki wantedSuit

                else
                    randomRyanmentenWithNobetan wantedSuit
            )


random4SidedWaitTwoSuits : Random.Generator (List Tile.Tile)
random4SidedWaitTwoSuits =
    Random.pair Suit.randomTwoHonorSuits (Random.uniform True [ False ])
        |> Random.andThen
            (\( ( suit1, suit2 ), b ) ->
                if b then
                    randomSanmenchanWithShanpon suit1 suit2

                else
                    randomDoubleEntotsu suit1 suit2
            )


{-| Example: 6667888p, waits 56789p
-}
randomTatsumaki : RandomSuitPreference -> Random.Generator (List Tile.Tile)
randomTatsumaki wantedSuit =
    let
        baseSuit =
            case wantedSuit of
                OneSuitFixed s ->
                    Random.constant s

                OneRandomSuit ->
                    Suit.randomNonHonorSuit

                _ ->
                    Suit.randomNonHonorSuit
    in
    baseSuit
        |> Random.andThen (\s -> Random.pair (Random.constant s) (Random.int 2 5))
        |> Random.map
            (\( s, n ) ->
                [ Tile.Tile n s
                , Tile.Tile n s
                , Tile.Tile n s
                , Tile.Tile (n + 1) s
                , Tile.Tile (n + 2) s
                , Tile.Tile (n + 2) s
                , Tile.Tile (n + 2) s
                ]
            )


{-| Example: 3334567m, waits 24578p
-}
randomRyanmentenWithNobetan : RandomSuitPreference -> Random.Generator (List Tile.Tile)
randomRyanmentenWithNobetan wantedSuit =
    let
        baseSuit =
            case wantedSuit of
                OneSuitFixed s ->
                    Random.constant s

                OneRandomSuit ->
                    Suit.randomNonHonorSuit

                _ ->
                    Suit.randomNonHonorSuit

        tripletPart numA numB suit =
            Random.uniform (Tile.Tile numA suit) [ Tile.Tile numB suit ]
                |> Random.map (\t -> List.repeat 2 t)
    in
    Random.pair baseSuit (Random.int 2 5)
        |> Random.andThen
            (\( s, n ) ->
                Random.pair (tripletPart n (n + 4) s)
                    (Random.constant
                        [ Tile.Tile n s
                        , Tile.Tile (n + 1) s
                        , Tile.Tile (n + 2) s
                        , Tile.Tile (n + 3) s
                        , Tile.Tile (n + 4) s
                        ]
                    )
            )
        |> Random.map
            (\( tp, tiles ) ->
                List.append tp tiles
                    |> Tile.sort
            )


{-| Example: 78999p56777s, waits 69p47s
-}
randomSanmenchanWithShanpon : Suit.Suit -> Suit.Suit -> Random.Generator (List Tile.Tile)
randomSanmenchanWithShanpon suit1 suit2 =
    let
        helper : Suit.Suit -> Random.Generator (List Tile.Tile)
        helper suit =
            Random.map2
                (\n tripletStart ->
                    if tripletStart then
                        [ Tile.Tile n suit
                        , Tile.Tile n suit
                        , Tile.Tile n suit
                        , Tile.Tile (n + 1) suit
                        , Tile.Tile (n + 2) suit
                        , Tile.Tile (n + 3) suit
                        , Tile.Tile (n + 4) suit
                        , Tile.Tile (n + 5) suit
                        ]

                    else
                        [ Tile.Tile (n + 1) suit
                        , Tile.Tile (n + 2) suit
                        , Tile.Tile (n + 3) suit
                        , Tile.Tile (n + 4) suit
                        , Tile.Tile (n + 5) suit
                        , Tile.Tile (n + 6) suit
                        , Tile.Tile (n + 6) suit
                        , Tile.Tile (n + 6) suit
                        ]
                )
                (Random.int 1 3)
                (Random.uniform True [ False ])

        pair =
            randomPairOf suit2
                |> Random.map toTiles
    in
    Random.map2 (\t1 t2 -> List.append t1 t2) (helper suit1) pair


randomDoubleEntotsu : Suit.Suit -> Suit.Suit -> Random.Generator (List Tile.Tile)
randomDoubleEntotsu suit1 suit2 =
    let
        entotsu suit =
            Random.map2
                (\n tripletStart ->
                    if tripletStart then
                        [ Tile.Tile n suit
                        , Tile.Tile n suit
                        , Tile.Tile n suit
                        , Tile.Tile (n + 1) suit
                        , Tile.Tile (n + 2) suit
                        ]

                    else
                        [ Tile.Tile (n + 1) suit
                        , Tile.Tile (n + 2) suit
                        , Tile.Tile (n + 3) suit
                        , Tile.Tile (n + 3) suit
                        , Tile.Tile (n + 3) suit
                        ]
                )
                (Random.int 1 6)
                (Random.uniform True [ False ])
    in
    Random.map2 List.append (entotsu suit1) (entotsu suit2)


winningTiles : List Tile.Tile -> List ( Tile.Tile, List Group )
winningTiles tiles =
    if List.member (List.length tiles) [ 4, 7, 10, 13 ] then
        let
            generateHand : ( Tile.Tile, List Tile.Tile ) -> ( Tile.Tile, List Tile.Tile, List Group )
            generateHand ( tile, listTiles ) =
                ( tile, listTiles, findWinningGroups (findGroups listTiles) )
        in
        List.map (\t -> ( t, Tile.push t tiles )) Tile.allTiles
            |> List.map generateHand
            |> List.filter (\( _, tt, gg ) -> isWinningHand tt gg)
            |> List.map (\( t, _, gg ) -> ( t, gg ))

    else
        []


isWinningHand : List Tile.Tile -> List Group -> Bool
isWinningHand tiles groups =
    let
        numTiles =
            List.length tiles

        enoughTiles =
            List.member numTiles [ 5, 8, 11, 14 ]

        enoughGroups =
            List.length groups == (numTiles + 1) // 3
    in
    enoughTiles && enoughGroups && not (Tile.hasMoreThan4Tiles tiles)


member : Tile.Tile -> Group -> Bool
member tile group =
    List.member tile (toTiles group)
