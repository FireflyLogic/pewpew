module PewPew.Level where

import PewPew.Model (Enemy, makeEnemy, enemyVelocity)
import PewPew.Utils (..)


parseLine: (Int, String) -> [(Int,Int)]
parseLine (row, chars) =
   let nonBlank c = c /= " "
       withRow col = (row,col)
   in chars
       |> String.split ""
       |> withIndex
       |> filter (nonBlank . snd)
       |> map (withRow . fst)

asciiToEnemies: String -> [Enemy]
asciiToEnemies s =
    let lines = s |> String.split "\n"
        positions = lines |> withIndex |> concatMap parseLine
        count = length positions
        initialVelocity = enemyVelocity 1 count
    in positions |> map (\(row,col) -> makeEnemy row col initialVelocity)

level = """
  *      **   *
  * *   *  *  * *
  * *   *  *  * *
  ****  *  *  ****
    *   *  *    *
    *    **     *
"""

create : () -> [Enemy]
create () =
    asciiToEnemies level
