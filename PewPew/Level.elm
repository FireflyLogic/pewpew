module PewPew.Level where

import PewPew.Model as Model
import PewPew.Utils as Utils


parseLine: (Int, String) -> [(Int,Int)]
parseLine (row, chars) =
   let nonBlank c = c /= " "
       withRow col = (row,col)

   in chars
       |> String.split ""
       |> Utils.withIndex
       |> filter (nonBlank . snd)
       |> map (withRow . fst)


asciiToEnemies: String -> [Model.Enemy]
asciiToEnemies string =
    let lines = String.split "\n" string
        positions = lines
            |> Utils.withIndex
            |> concatMap parseLine
        count = length positions
        initialVelocity = Model.enemyVelocity 1 count
        makeEnemy (row,col) = Model.makeEnemy row col initialVelocity

    in map makeEnemy positions


level = """
  *      **   *
  * *   *  *  * *
  * *   *  *  * *
  ****  *  *  ****
    *   *  *    *
    *    **     *
"""


create : () -> [Model.Enemy]
create () =
    asciiToEnemies level
