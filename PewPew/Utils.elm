module PewPew.Utils where

import List exposing (..)
import Signal
import Time exposing (..)

withIndex list = list |> map2 (,) [0..length list]

throttle: Signal Bool -> Time -> Signal Bool
throttle input interval=
    let hz = fps 60
        sampled = timestamp <| Signal.sampleOn hz input
        throttle' (t,input) (_, tLast) =
          if input && t-tLast > interval then (True,t)
          else (False, tLast)
    in
        Signal.map fst (Signal.foldp throttle' (False,0) sampled)

cubicEasing: Int -> Float -> Float -> Int -> Float
cubicEasing duration min max t =
   let t' = (toFloat t) / (toFloat duration/2.0) in
     if t' < 1 then
       (max-min)/2.0 * t' ^ 3 + min
     else
       let t'' = t' - 2.0
       in (max-min)/2.0 * (t''^3 + 2.0) + min


-- From Pong in Elm sample (http://elm-lang.org/edit/examples/Intermediate/Pong.elm)
near : Float -> Float -> Float -> Bool
near n c m = m >= n-c && m <= n+c
