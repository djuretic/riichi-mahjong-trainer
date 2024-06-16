module Anim exposing (tick)

import Time


type alias ModelAnim a b =
    { a | lastTick : Int, animatedTiles : b }


tick : Time.Posix -> (b -> b) -> ModelAnim a b -> ModelAnim a b
tick tickTime doAnimationFn model =
    let
        fps =
            30

        fpsInterval =
            1000 / fps

        now =
            Time.posixToMillis tickTime

        elapsed =
            now - model.lastTick
    in
    if model.lastTick == 0 then
        { model | lastTick = now }

    else if toFloat elapsed > fpsInterval then
        { model | lastTick = now - remainderBy (round fpsInterval) elapsed, animatedTiles = doAnimationFn model.animatedTiles }

    else
        model
