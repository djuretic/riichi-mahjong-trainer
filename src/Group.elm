module Group exposing
    ( Group
    , GroupType(..)
    , GroupsPerSuit
    , containsTerminal
    , findGroups
    , isDragon
    , isPair
    , isRun
    , isTriplet
    , toString
    , toTiles
    , toWind
    )

import Array
import Tile


type GroupType
    = Triplet
    | Run
    | Pair


type alias Group =
    { type_ : GroupType

    -- for runs, is the first (lowest) tile
    , tileNumber : Tile.TileNumber
    , suit : Tile.Suit
    }


type alias GroupsPerSuit =
    { sou : List (List Group)
    , man : List (List Group)
    , pin : List (List Group)
    , honor : List (List Group)
    }


type alias Counter =
    Array.Array Int


toWind : Group -> Maybe Tile.Wind
toWind group =
    let
        getWind g =
            if g.suit == Tile.Honor then
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


findGroups : List Tile.Tile -> GroupsPerSuit
findGroups tiles =
    let
        part =
            Tile.partitionBySuit tiles

        findAllGroups : List Tile.Tile -> Tile.Suit -> List (List Group)
        findAllGroups =
            \t suit ->
                List.map .number t
                    |> List.sort
                    |> toArrayCounter
                    |> findGroupsInSuit suit 0 True
                    |> Maybe.withDefault []

        groupsPerSuit =
            { sou = findAllGroups part.sou Tile.Sou
            , man = findAllGroups part.man Tile.Man
            , pin = findAllGroups part.pin Tile.Pin
            , honor = findAllGroups part.honor Tile.Honor
            }
    in
    groupsPerSuit


toArrayCounter : List Tile.TileNumber -> Counter
toArrayCounter tileNumbers =
    let
        counter =
            Array.initialize 9 (always 0)

        accum : Tile.TileNumber -> Array.Array Int -> Array.Array Int
        accum n cnt =
            Array.set (n - 1) (Maybe.withDefault 0 (Array.get (n - 1) cnt) + 1) cnt
    in
    List.foldl accum counter tileNumbers


getCount : Int -> Counter -> Int
getCount n counter =
    Array.get n counter
        |> Maybe.withDefault 0


findGroupsInSuit : Tile.Suit -> Int -> Bool -> Counter -> Maybe (List (List Group))
findGroupsInSuit suit n shouldFindPair counter =
    let
        count =
            Array.get n counter
                |> Maybe.withDefault 0
    in
    if n >= Array.length counter then
        Just [ [] ]

    else if count == 0 then
        findGroupsInSuit suit (n + 1) shouldFindPair counter

    else
        let
            foundTriplet =
                count >= 3

            triplet =
                if foundTriplet then
                    findGroupsInSuit suit n shouldFindPair (Array.set n (count - 3) counter)
                        |> addGroupToHead (Group Triplet (n + 1) suit)

                else
                    Nothing

            foundPair =
                count >= 2

            pair =
                if foundPair && shouldFindPair then
                    findGroupsInSuit suit n False (Array.set n (count - 2) counter)
                        |> addGroupToHead (Group Pair (n + 1) suit)

                else
                    Nothing

            foundRun =
                suit /= Tile.Honor && n < 7 && count >= 1 && getCount (n + 1) counter > 0 && getCount (n + 2) counter > 0

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
                    findGroupsInSuit suit n shouldFindPair updatedCounter
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
            String.repeat 3 (String.fromInt group.tileNumber) ++ Tile.suitToString group.suit

        Pair ->
            String.repeat 2 (String.fromInt group.tileNumber) ++ Tile.suitToString group.suit

        Run ->
            String.join ""
                [ String.fromInt group.tileNumber
                , String.fromInt (group.tileNumber + 1)
                , String.fromInt (group.tileNumber + 2)
                , Tile.suitToString group.suit
                ]


isTriplet : Group -> Bool
isTriplet group =
    group.type_ == Triplet


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
    isTripletOrPair && group.suit == Tile.Honor && List.member group.tileNumber [ Tile.whiteDragonNumber, Tile.greenDragonNumber, Tile.redDragonNumber ]


containsTerminal : Group -> Bool
containsTerminal group =
    if group.suit == Tile.Honor then
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
