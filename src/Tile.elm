module Tile exposing
    ( Group
    , GroupType(..)
    , GroupsPerSuit
    , Suit(..)
    , Tile
    , Wind(..)
    , containsTerminal
    , findGroups
    , groupIsPair
    , groupIsRun
    , groupIsTriplet
    , groupToString
    , groupToWind
    , isDragon
    , suitToString
    , windToString
    )

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
