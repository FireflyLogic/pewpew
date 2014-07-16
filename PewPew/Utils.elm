module PewPew.Utils where

withIndex list = list |> zip [0..length list]

throttle: Signal Bool -> Time -> Signal Bool
throttle input interval=
    let hz = fps 60
        sampled = timestamp <| sampleOn hz input
        throttle' (t,input) (_, tLast) = if
            | input && t-tLast > interval  -> (True,t)
            | otherwise -> (False, tLast)
    in
        fst <~ foldp throttle' (False,0) sampled
