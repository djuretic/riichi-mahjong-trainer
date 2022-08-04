module Counter exposing (Counter, getCount, hasCountGreaterThan)

import Array


type alias Counter =
    Array.Array Int


getCount : Int -> Counter -> Int
getCount n counter =
    Array.get n counter
        |> Maybe.withDefault 0


hasCountGreaterThan : Int -> Counter -> Bool
hasCountGreaterThan n counter =
    let
        finds =
            Array.filter (\i -> i > n) counter
    in
    Array.length finds > 0
