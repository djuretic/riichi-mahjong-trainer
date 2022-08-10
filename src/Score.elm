module Score exposing (Score, score)


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


score : Int -> Int -> Score
score hanCount fuCount =
    if hanCount >= 13 then
        Score (DealerScore 48000 16000) (NonDealerScore 32000 16000 8000)

    else if hanCount >= 11 then
        Score (DealerScore 36000 12000) (NonDealerScore 24000 12000 6000)

    else if hanCount >= 8 then
        Score (DealerScore 24000 8000) (NonDealerScore 16000 8000 4000)

    else if hanCount >= 6 then
        Score (DealerScore 18000 6000) (NonDealerScore 12000 6000 3000)

    else if hanCount >= 5 then
        Score (DealerScore 18000 6000) (NonDealerScore 12000 6000 3000)

    else
        case ( hanCount, fuCount ) of
            ( 1, 30 ) ->
                Score (DealerScore 1500 500) (NonDealerScore 1000 500 300)

            ( 2, 20 ) ->
                Score (DealerScore 2000 700) (NonDealerScore 1300 700 400)

            ( 2, 40 ) ->
                score 3 20

            ( 3, 20 ) ->
                Score (DealerScore 3900 1300) (NonDealerScore 2600 1300 700)

            ( 3, 40 ) ->
                score 4 20

            ( 4, 20 ) ->
                Score (DealerScore 7700 2600) (NonDealerScore 5200 2600 1300)

            _ ->
                Score (DealerScore 0 0) (NonDealerScore 0 0 0)
