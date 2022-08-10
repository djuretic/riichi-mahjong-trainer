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

            ( 2, 25 ) ->
                Score (DealerScore 2400 800) (NonDealerScore 1600 800 400)

            ( 2, 30 ) ->
                Score (DealerScore 2900 1000) (NonDealerScore 2000 1000 500)

            ( 3, 20 ) ->
                Score (DealerScore 3900 1300) (NonDealerScore 2600 1300 700)

            ( 3, 25 ) ->
                Score (DealerScore 4800 1600) (NonDealerScore 3200 1600 800)

            ( 4, 20 ) ->
                Score (DealerScore 7700 2600) (NonDealerScore 5200 2600 1300)

            ( 4, 25 ) ->
                Score (DealerScore 9600 3200) (NonDealerScore 6400 3200 1600)

            ( 4, 30 ) ->
                Score (DealerScore 11600 3900) (NonDealerScore 7700 3900 2000)

            _ ->
                if hanCount == 4 && fuCount > 30 then
                    score 5 0

                else if hanCount == 3 && fuCount > 60 then
                    score 5 0

                else if List.member fuCount [ 40, 50, 60 ] then
                    score (hanCount + 1) (fuCount // 2)

                else if fuCount > 60 then
                    sumScores (score hanCount 50) (score hanCount (fuCount - 50))

                else
                    Score (DealerScore 0 0) (NonDealerScore 0 0 0)


sumScores : Score -> Score -> Score
sumScores s1 s2 =
    Score (sumDealer s1.dealer s2.dealer) (sumNonDealer s1.nonDealer s2.nonDealer)


sumDealer : DealerScore -> DealerScore -> DealerScore
sumDealer s1 s2 =
    DealerScore (s1.ron + s2.ron) (s1.tsumo + s2.tsumo)


sumNonDealer : NonDealerScore -> NonDealerScore -> NonDealerScore
sumNonDealer s1 s2 =
    NonDealerScore (s1.ron + s2.ron) (s1.tsumoDealer + s2.tsumoDealer) (s1.tsumoNonDealer + s2.tsumoNonDealer)
