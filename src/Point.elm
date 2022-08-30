module Point exposing (Point, easing, setX)

import Ease


type alias Point =
    ( Int, Int )


easingSteps : Int
easingSteps =
    20


easing : Point -> Point -> List Point
easing initial final =
    let
        ratios =
            List.map (\ii -> Ease.outCubic (toFloat ii / toFloat easingSteps)) (List.range 0 easingSteps)
    in
    List.map
        (\r ->
            let
                ( initialX, initialY ) =
                    initial

                ( finalX, finalY ) =
                    final
            in
            ( (1 - r) * toFloat initialX + r * toFloat finalX |> round
            , (1 - r) * toFloat initialY + r * toFloat finalY |> round
            )
        )
        ratios


setX : Int -> Point -> Point
setX x point =
    ( x, Tuple.second point )
