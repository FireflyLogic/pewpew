module PewPew.Input where

import Keyboard
import Touch
import PewPew.Utils as Utils

type Input = {
    firing:Bool,
    direction:Int,
    delta:Time
}

delta = inSeconds <~ fps 60

touchFire touches =
    (length touches) > 1 ||
    any (\ {x,x0} -> (x0-x)==0 ) touches

firing = merge Keyboard.space (touchFire <~ Touch.touches)


touchMove touches =
    let directionSignals = touches
        |> filter (\{x,x0} -> abs (x0-x) > 2)
        |> map (\{x,x0} -> if x<x0 then -1 else 1 )
    in case directionSignals of
        head::tail -> head
        [] -> 0

direction = merge (.x <~ Keyboard.arrows) (touchMove <~ Touch.touches)

input =
    sampleOn delta (Input <~
        Utils.throttle firing (350 * millisecond)
       ~ direction
       ~ delta)
