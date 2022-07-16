module Tile exposing (Group, GroupType(..), GroupsPerSuit, Suit(..), Tile, findGroups)

import List.Extra exposing (permutations)


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
