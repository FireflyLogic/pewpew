module PewPew.Input where

import Keyboard

type Input = { firing:Bool, direction:Int, delta:Time }

delta = inSeconds <~ fps 60

throttle: Signal Bool -> Time -> Signal Bool
throttle input interval=
    let signal = timestamp <| sampleOn delta input
        throttle (t,input) (_, tLast) =
            if
                | input && t-tLast > interval  -> (True,t)
                | otherwise -> (False, tLast)
    in
        fst <~ foldp throttle (False,0) signal

input =
    sampleOn delta (Input <~
        throttle Keyboard.space (350 * millisecond)
       ~ lift .x Keyboard.arrows
       ~ delta)
