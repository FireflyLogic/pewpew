module PewPew.Input where

import Keyboard
import PewPew.Utils as Utils

type Input = {
    firing:Bool,
    direction:Int,
    delta:Time
}

delta = inSeconds <~ fps 60

input =
    sampleOn delta (Input <~
        Utils.throttle Keyboard.space (350 * millisecond)
       ~ lift .x Keyboard.arrows
       ~ delta)
