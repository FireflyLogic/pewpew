module PewPew.Input where

import Keyboard
import Touch
import PewPew.Utils as Utils

type Input = {
    firing:Bool,
    direction:Int,
    delta:Time
}

touchFire: [Touch.Touch] -> Bool
touchFire touches =
    (length touches) > 1 ||
    any (\ {x,x0} -> (x0-x)==0 ) touches


touchMove: [Touch.Touch] -> Int
touchMove touches =
    let directionSignals = touches
        |> filter (\{x,x0} -> abs (x0-x) > 2)
        |> map (\{x,x0} -> if x<x0 then -1 else 1 )
    in case directionSignals of
        h::t -> h
        []   -> 0

delta = inSeconds <~ fps 60
firing = merge Keyboard.space (touchFire <~ Touch.touches)
direction = merge (.x <~ Keyboard.arrows) (touchMove <~ Touch.touches)

input =
    sampleOn delta (Input <~
        Utils.throttle firing (350 * millisecond)
       ~ direction
       ~ delta)
