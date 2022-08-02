module Tile exposing
    ( Group
    , GroupType(..)
    , GroupsPerSuit
    , Suit(..)
    , Tile
    , Wind(..)
    , containsTerminal
    , findGroups
    , findGroups2
    , greenDragonNumber
    , groupIsPair
    , groupIsRun
    , groupIsTriplet
    , groupToString
    , groupToWind
    , isDragon
    , redDragonNumber
    , suitToString
    , whiteDragonNumber
    , windToString
    , windToTileNumber
    )

import Array
import List.Extra exposing (permutations)


type Suit
    = Sou
    | Man
    | Pin
    | Honor


type Wind
    = East
    | South
    | West
    | North


type alias TileNumber =
    Int


type alias Tile =
    { number : TileNumber
    , suit : Suit
    }


redDragonNumber : TileNumber
redDragonNumber =
    5


greenDragonNumber : TileNumber
greenDragonNumber =
    6


whiteDragonNumber : TileNumber
whiteDragonNumber =
    7


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


type alias Counter =
    Array.Array Int


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


findGroups2 : List Tile -> GroupsPerSuit
findGroups2 tiles =
    let
        part =
            partitionBySuit tiles

        findAllGroups : List Tile -> Suit -> List (List Group)
        findAllGroups =
            \t suit ->
                List.map .number t
                    |> List.sort
                    |> toArrayCounter
                    |> findGroupsInSuit2 suit 0 True
                    |> Maybe.withDefault []

        groupsPerSuit =
            { sou = findAllGroups part.sou Sou
            , man = findAllGroups part.man Man
            , pin = findAllGroups part.pin Pin
            , honor = findAllGroups part.honor Honor
            }
    in
    groupsPerSuit


toArrayCounter : List TileNumber -> Counter
toArrayCounter tileNumbers =
    let
        counter =
            Array.initialize 9 (always 0)

        accum : TileNumber -> Array.Array Int -> Array.Array Int
        accum n cnt =
            Array.set (n - 1) (Maybe.withDefault 0 (Array.get (n - 1) cnt) + 1) cnt
    in
    List.foldl accum counter tileNumbers


getCount : Int -> Counter -> Int
getCount n counter =
    Array.get n counter
        |> Maybe.withDefault 0


findGroupsInSuit2 : Suit -> Int -> Bool -> Counter -> Maybe (List (List Group))
findGroupsInSuit2 suit n shouldFindPair counter =
    let
        count =
            Array.get n counter
                |> Maybe.withDefault 0
    in
    if n >= Array.length counter then
        Just [ [] ]

    else if count == 0 then
        findGroupsInSuit2 suit (n + 1) shouldFindPair counter

    else
        let
            foundTriplet =
                count >= 3

            triplet =
                if foundTriplet then
                    findGroupsInSuit2 suit n shouldFindPair (Array.set n (count - 3) counter)
                        |> addGroupToHead (Group Triplet (n + 1) suit)

                else
                    Nothing

            foundPair =
                count >= 2

            pair =
                if foundPair && shouldFindPair then
                    findGroupsInSuit2 suit n False (Array.set n (count - 2) counter)
                        |> addGroupToHead (Group Pair (n + 1) suit)

                else
                    Nothing

            foundRun =
                suit /= Honor && n < 7 && count >= 1 && getCount (n + 1) counter > 0 && getCount (n + 2) counter > 0

            run =
                if foundRun then
                    let
                        count2 =
                            getCount (n + 1) counter

                        count3 =
                            getCount (n + 2) counter

                        updatedCounter =
                            counter
                                |> Array.set n (count - 1)
                                |> Array.set (n + 1) (count2 - 1)
                                |> Array.set (n + 2) (count3 - 1)
                    in
                    findGroupsInSuit2 suit n shouldFindPair updatedCounter
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


isRun : List TileNumber -> Bool
isRun tiles =
    case tiles of
        x :: y :: [ z ] ->
            x + 1 == y && y + 1 == z

        _ ->
            False


isTriplet : List TileNumber -> Bool
isTriplet tiles =
    case tiles of
        x :: y :: [ z ] ->
            x == y && y == z

        _ ->
            False


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
    let
        isTripletOrPair =
            groupIsTriplet group || groupIsPair group
    in
    isTripletOrPair && group.suit == Honor && List.member group.tileNumber [ whiteDragonNumber, greenDragonNumber, redDragonNumber ]


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


windToTileNumber : Wind -> TileNumber
windToTileNumber wind =
    case wind of
        East ->
            1

        South ->
            2

        West ->
            3

        North ->
            4
