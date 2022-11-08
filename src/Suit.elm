module Suit exposing (Suit(..), maxRange, randomNonHonorSuit, randomSuit, toString, fromString)

import Random


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


maxRange : Suit -> Int
maxRange suit =
    if suit == Honor then
        7

    else
        9
