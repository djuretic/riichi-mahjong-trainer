module Suit exposing (Suit(..), fromString, maxRange, randomNonHonorSuit, randomSuit, randomTwoHonorSuits, toString)

import Random
import Random.List


type Suit
    = Sou
    | Man
    | Pin
    | Honor


fromString : String -> Maybe Suit
fromString s =
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


toString : Suit -> String
toString suit =
    case suit of
        Man ->
            "m"

        Pin ->
            "p"

        Sou ->
            "s"

        Honor ->
            "z"


randomSuit : Random.Generator Suit
randomSuit =
    Random.uniform Man [ Pin, Sou, Honor ]


randomNonHonorSuit : Random.Generator Suit
randomNonHonorSuit =
    Random.uniform Man [ Pin, Sou ]


{-| Two different suits
-}
randomTwoHonorSuits : Random.Generator ( Suit, Suit )
randomTwoHonorSuits =
    Random.List.choices 2 [ Man, Pin, Sou ]
        |> Random.map
            (\( suits, _ ) ->
                case suits of
                    [ s1, s2 ] ->
                        ( s1, s2 )

                    _ ->
                        ( Man, Pin )
            )


maxRange : Suit -> Int
maxRange suit =
    if suit == Honor then
        7

    else
        9
