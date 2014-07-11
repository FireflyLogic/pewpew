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
        throttle Keyboard.space (350 * millisecond)
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
type Enemy = Object {}
type Explosion = Object { time: Time }
type Projectile = Object {}
type Collision = (Object, Object)

type Game = {
    --score: Int,
    --state: State,
    ship: Ship,
    projectiles: [Projectile],
    enemies: [Enemy],
    explosions: [Explosion]
    -- enemyProjectiles: [Projectile]
    }

withIndex list = list |> zip [0..length list]

enemyVelocity: Int -> Int -> Float
enemyVelocity dir enemiesRemaining =
    (toFloat dir) / (toFloat enemiesRemaining) * 1000

makeEnemy: number -> number -> String -> Maybe Enemy
makeEnemy row col c=
   let y =  200 - (row * size)
       x = (col * size) -  300
   in
     case c of
       "*" -> Just { x=x, y=y, vx=1, vy=0}
       _   -> Nothing

parseLine: (number,String) -> [Enemy]
parseLine (row, chars) =
   chars |> String.split "" |> withIndex |> map (\(col,char)-> makeEnemy row col char) |> justs

asciiToEnemies: String -> [Enemy]
asciiToEnemies s =
 let lines   = s |> String.split "\n"
     enemies = lines |> withIndex |> concatMap parseLine
     count = length enemies
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
    enemies          = asciiToEnemies level,
    explosions       = []
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
        vx'       = toFloat dir * 400
        ship'     = stepObj t { ship | vx <- vx'  }
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
stepEnemies t enemies =
    let enemies'   = enemies |> map (stepObj t)
        count      = length enemies'
        positions  = map .x enemies'
        (low,high) = (minimum positions, maximum positions)
        dir        = if
                        | low + halfWidth <= 30 -> Just 1
                        | halfWidth - high < 30 -> Just -1
                        | otherwise  -> Nothing

    in case dir of
         Just v -> map (\enemy -> { enemy | vx <- enemyVelocity v count }) enemies'
         _      -> enemies'

-- are n and m near each other?
-- specifically are they within c of each other?
near : Float -> Float -> Float -> Bool
near n c m = m >= n-c && m <= n+c

-- is the ball within a paddle?
within : (Projectile,Enemy) -> Bool
within (projectile, enemy) =
    (projectile.x |> near enemy.x 14) && (projectile.y |> near enemy.y 8)

except: [a] -> [a] -> [a]
except a b =
    let inB x = any ((==) x) b
    in filter (not . inB) a

stepCollisions: [Projectile] -> [Enemy] -> ([Projectile],[Enemy],[Explosion])
stepCollisions projectiles enemies =
    let hits = projectiles |> concatMap ((flip map enemies) . (,)) |> filter within
        (hitProjectiles, hitEnemies) = unzip hits
        explosions = hitEnemies |> map (\enemy -> {enemy | time = inSeconds 100 * millisecond})
    in
        (projectiles `except` hitProjectiles, enemies `except` hitEnemies, explosions)

stepExplosions: Time -> [Explosion] -> [Explosion]
stepExplosions t explosions =
    let burn e = {e | time <- e.time - t}
    in explosions |> map (stepObj t) |> map burn |> filter ((<) 0 . .time)


step : Input -> Game -> Game
step {firing, direction, delta} ({ship, projectiles, enemies, explosions} as game)=
    let ship'        = stepShip delta direction ship
        projectiles' = stepProjectiles delta firing ship.x projectiles
        enemies'     = stepEnemies delta enemies
        (projectiles'',enemies'', explosions') = stepCollisions projectiles' enemies'
        explosions'' = stepExplosions delta explosions ++ explosions'
    in
        {game |
            ship <- ship',
            projectiles <- projectiles'',
            enemies <- enemies'',
            explosions <- explosions''
        }




{-- View --}

-- helper values
starField = rgb 0 0 0

-- shared function for rendering objects
displayObj :  Shape -> Object a -> Form
displayObj shape obj =
    move (obj.x,obj.y) (filled white shape)

displayEnemy: Enemy -> Form
displayEnemy enemy =
    toForm (fittedImage 30 30 "/assets/red-2.png") |> move (enemy.x, enemy.y) |> rotate (degrees 180)

displayExplosion: Explosion -> Form
displayExplosion boom =
    toForm (fittedImage 30 30 "/assets/explosion.png") |> move (boom.x, boom.y) |> rotate (degrees 180)


-- display a game state
display : (Int,Int) -> Game -> Element
display (w,h) ({ship, projectiles, enemies, explosions} as game) =
    let objs = [
       filled starField   (rect gameWidth gameHeight),
       toForm (fittedImage 40 40 "/assets/ship.png") |> move (ship.x, ship.y)
    ] ++ (map (displayObj (rect 2 6)) projectiles) ++ (map displayEnemy enemies) ++ (map displayExplosion explosions)

    in
        layers [
        --container w h midTop <| asText game,
        container w h middle <|
            collage gameWidth gameHeight objs
            ]

{-- Run --}
game = foldp step defaultGame input
main =  display <~ Window.dimensions ~ game
