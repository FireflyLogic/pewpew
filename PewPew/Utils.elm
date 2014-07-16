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

cubicEasing: Int -> Float -> Float -> Int -> Float
cubicEasing duration min max t =
   let t' = (toFloat t) / (toFloat duration/2.0)
   in if
      | t' < 1 -> (max-min)/2.0 * t' ^ 3 + min
      | otherwise ->
         let t'' = t' - 2.0
         in (max-min)/2.0 * (t''^3 + 2.0) + min
