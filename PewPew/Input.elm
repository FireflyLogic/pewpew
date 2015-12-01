module PewPew.Input where

import Keyboard
import List exposing (..)
import PewPew.Utils as Utils
import Signal
import Time exposing (..)
import Touch

type alias Input = {
    firing:Bool,
    direction:Int,
    delta:Time
}

touchFire: List Touch.Touch -> Bool
touchFire touches =
    (length touches) > 1 ||
    any (\ {x,x0} -> (x0-x)==0 ) touches


touchMove: List Touch.Touch -> Int
touchMove touches =
    let directionSignals = touches
        |> filter (\{x,x0} -> abs (x0-x) > 2)
        |> map (\{x,x0} -> if x<x0 then -1 else 1 )
    in case directionSignals of
        h::t -> h
        []   -> 0

delta = Signal.map inSeconds (fps 60)
firing = Signal.merge Keyboard.space (Signal.map touchFire Touch.touches)
direction = Signal.merge (Signal.map (.x) Keyboard.arrows) (Signal.map touchMove Touch.touches)

input =
    Signal.sampleOn delta (Signal.map3 Input
        (Utils.throttle firing (350 * millisecond))
        direction
        delta)
