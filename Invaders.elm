module PewPew where

import Keyboard
import Text
import Window
import String

{-- Inputs --}

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
        throttle Keyboard.space (250 * millisecond)
       ~ lift .x Keyboard.arrows
       ~ delta)



{-- Model --}

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (gameWidth/2,gameHeight/2)
padding = 4
size = 30


--data State = Start | Play | End

type Object a = { a | x:Float, y:Float, vx:Float, vy:Float }

type Ship = Object {}
type Enemy = Object { hits:Int }
type Projectile = Object {}

type Game = {
    --score: Int,
    --state: State,
    ship: Ship,
    projectiles: [Projectile],
    enemies: [Enemy]
    -- enemyProjectiles: [Projectile]
    }

withIndex list = list |> zip [0..length list]

enemyVelocity: number -> number -> Float
enemyVelocity v count =
    v / count * 1000


makeEnemy: number -> number -> String -> Maybe Enemy
makeEnemy row col c=
   let y =  200 - (row * size)
       x = (col * size) -  300
   in
     case c of
       "*" -> Just { x=x, y=y, vx=1, vy=0, hits=1 }
       _   -> Nothing

parseLine: (number,String) -> [Enemy]
parseLine (row, chars) =
   chars |> String.split "" |> withIndex |> map (\(col,char)-> makeEnemy row col char) |> justs

asciiToEnemies: String -> [Enemy]
asciiToEnemies s =
 let lines   = s |> String.split "\n"
     enemies = lines |> withIndex |> concatMap parseLine
     count = toFloat <| length enemies
 in enemies |> map (\enemy -> {enemy | vx <- enemyVelocity 1 count})

level = """
  *      **   *
  * *   *  *  * *
  * *   *  *  * *
  ****  *  *  ****
    *   *  *    *
    *    **     *
"""

defaultGame : Game
defaultGame = {
    --score            = 0,
    --state            = Start,
    ship             = { x=-halfWidth, y=20-halfHeight, vx = 0, vy=0},
    projectiles      = [],
    enemies          = asciiToEnemies level
    -- enemyProjectiles = []
    }



{-- Updates --}

stepObj : Time -> Object a -> Object a
stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx * t
          , y <- y + vy * t }

stepShip : Time -> Int -> Ship -> Ship
stepShip t dir ship =
    let shipWidth = 40
        ship'     = stepObj t { ship | vx <- toFloat dir * 600 }
        x'        = clamp (shipWidth/2-halfWidth) (halfWidth-shipWidth/2) ship'.x
  in
      { ship' | x <- x'}

isOnScreen projectile = projectile.y < halfHeight

stepProjectiles : Time -> Bool -> Float -> [Projectile] -> [Projectile]
stepProjectiles t firing origin projectiles =
    let projectiles' =  projectiles |> map (stepObj t) |> filter isOnScreen
    in case firing of
        True -> { x=origin, y=20-halfHeight, vx = 0, vy=200} :: projectiles'
        _ -> projectiles'

stepEnemies : Time -> [Enemy] -> [Enemy]
stepEnemies t  enemies =
    let enemies'   =  enemies |> map (stepObj t)
        count      = toFloat <| length enemies'
        positions  = map .x enemies'
        (low,high) = (minimum positions, maximum positions)
        velocity   =
            if
                | low + halfWidth <= 30 -> Just 1
                | halfWidth - high < 30 -> Just -1
                | otherwise  -> Nothing

    in case velocity of
         Just v -> map (\enemy -> { enemy | vx <- enemyVelocity v count }) enemies'
         _      -> enemies'

stepGame : Input -> Game -> Game
stepGame {firing, direction, delta}
         ({ship, projectiles, enemies} as game)=
    let ship'        = stepShip delta direction ship
        projectiles' = stepProjectiles delta firing ship.x projectiles
        enemies'     = stepEnemies delta enemies
    in
        {game |
            ship <- ship',
            projectiles <- projectiles',
            enemies <- enemies'
        }




{-- View --}

-- display : (Int,Int) -> Game -> Element
-- display (w,h) gameState = asText gameState

-- helper values
starField = rgb 0 0 0

-- shared function for rendering objects
displayObj :  Shape -> Object a -> Form
displayObj shape obj =
    move (obj.x,obj.y) (filled white shape)

displayEnemy: Enemy -> Form
displayEnemy enemy =
    toForm (fittedImage 30 30 "/assets/red-2.png") |> move (enemy.x, enemy.y) |> rotate (degrees 180)


-- display a game state
display : (Int,Int) -> Game -> Element
display (w,h) ({ship, projectiles, enemies} as game) =
    let objs = [
       filled starField   (rect gameWidth gameHeight),
       toForm (fittedImage 40 40 "/assets/ship.png") |> move (ship.x, ship.y)
    ] ++ (map (displayObj (rect 2 6)) projectiles) ++ (map displayEnemy enemies)

    in
        layers [
        container w h midTop <| asText game,
        container w h middle <|
            collage gameWidth gameHeight objs
            ]

{-- Run --}
game = foldp stepGame defaultGame input
main =  display <~ Window.dimensions ~ game
