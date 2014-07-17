module PewPew.Step where

import PewPew.Input (Input)
import PewPew.Model (..)
import PewPew.Utils as Utils

--
--Helper functions
--

-- From Pong in Elm sample (http://elm-lang.org/edit/examples/Intermediate/Pong.elm)
stepObj : Time -> Object a -> Object a
stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx * t
          , y <- y + vy * t }

isOnScreen : Object a -> Bool
isOnScreen {x,y} =
    y < halfHeight
    && y > -halfHeight
    && x < halfWidth
    && x > -halfWidth

within : Float -> Float -> Object a -> Object b -> Bool
within x y a b =
     (a.x |> Utils.near b.x x) && (a.y |> Utils.near b.y y)

except: [a] -> [a] -> [a]
except a b =
    let inB x = any ((==) x) b
    in filter (not . inB) a


--
-- Model specific step functions
--

stepShip : Time -> Int -> Ship -> Ship
stepShip t dir ship =
    let shipWidth = 40
        vx' = toFloat dir * 400
        ship' = stepObj t { ship | vx <- vx'  }
        x' = clamp (shipWidth/2-halfWidth) (halfWidth-shipWidth/2) ship'.x

    in
      { ship' | x <- x'}


stepProjectiles : Time -> Bool -> Float -> [Projectile] -> [Projectile]
stepProjectiles t firing origin projectiles =
    let projectiles' =  projectiles
        |> map (stepObj t)
        |> filter isOnScreen

    in case firing of
        True -> { x=origin, y=20-halfHeight, vx = 0, vy=400 } :: projectiles'
        _ -> projectiles'


stepEnemies : Time -> [Enemy] -> [Enemy]
stepEnemies t enemies =
    let enemies' = enemies
                    |> map (stepObj t)
                    |> map (\e -> {e | lastFired <- e.lastFired + t})
        count = length enemies'
        positions = map .x enemies'
        (low,high) = (minimum positions, maximum positions)
        dir = if
                | low + halfWidth <= 30 -> Just 1
                | halfWidth - high < 30 -> Just -1
                | otherwise  -> Nothing

    in case dir of
         Just v -> map (\enemy -> { enemy | vx <- enemyVelocity v count }) enemies'
         _      -> enemies'


shouldFire : Int -> Enemy -> Int -> Bool
shouldFire enemiesRemaining enemy index =
    let interval = Utils.cubicEasing 34 1.0 10.0 enemiesRemaining
        wobble   = abs(tan(toFloat index)) * (toFloat enemiesRemaining) / 2

    in enemy.lastFired > interval + wobble


tryEnemyFire : Int -> (Int,Enemy) -> (Enemy, Maybe Projectile)
tryEnemyFire enemiesRemaing (index,enemy) =
    case shouldFire enemiesRemaing enemy index  of
    True  -> ({enemy| lastFired <- 0},
              Just {
                  x = enemy.x,
                  y = enemy.y,
                  vy = -100,
                  vx = 0 })
    False -> (enemy,Nothing)


stepEnemyFire : Time -> [Projectile] -> [Enemy] -> ([Enemy],[Projectile])
stepEnemyFire t projectiles enemies =
    let indexed = enemies |> Utils.withIndex
        projectiles' = projectiles
                        |> map (stepObj t)
                        |> filter isOnScreen

        (enemies',newProjectiles) = indexed
                                    |> map (tryEnemyFire (length enemies))
                                    |> unzip
        enemies'' = enemies' |> map (\enemy -> {enemy| lastFired <- enemy.lastFired + t})
        projectiles'' = (newProjectiles |> justs) ++ projectiles'

    in (enemies'', projectiles'')


stepEnemyCollisions: [Projectile] -> [Enemy] -> ([Projectile],[Enemy],[Explosion])
stepEnemyCollisions projectiles enemies =
    let hits = projectiles |> concatMap ((flip map enemies) . (,)) |> filter (uncurry (within 14 8))
        (hitProjectiles, hitEnemies) = unzip hits
        explosions = hitEnemies
            |> map (\enemy -> {
                time = inSeconds 150 * millisecond,
                vx = enemy.vx / 1.2 ,
                vy = 0,
                x = enemy.x,
                y = enemy.y
            })
    in
        (projectiles `except` hitProjectiles, enemies `except` hitEnemies, explosions)


stepExplosions: Time -> [Explosion] -> [Explosion]
stepExplosions t explosions =
    let burn e = {e | time <- e.time - t}
    in explosions
        |> map (stepObj t)
        |> map burn
        |> filter ((<) 0 . .time)


stepScore: Int -> Time -> Int -> Int
stepScore score duration enemiesHit =
    case enemiesHit of
        0 -> score
        _ -> score + enemiesHit * max (truncate (300 - duration)) 1

stepPlay : Input -> Game -> Game
stepPlay {firing, direction, delta} ({score, duration, ship, projectiles, enemies, explosions, enemyProjectiles} as game)=
    let ship' = stepShip delta direction ship
        projectiles' = stepProjectiles delta firing ship.x projectiles
        (enemies',enemyProjectiles') = case enemies of
            [] -> (enemies,enemyProjectiles)
            _  -> stepEnemies delta enemies |> (stepEnemyFire delta enemyProjectiles)
        (projectiles'',enemies'', explosions') = stepEnemyCollisions projectiles' enemies'
        explosions'' = stepExplosions delta explosions ++ explosions'
        duration' = duration + delta
        score' = stepScore score duration' (length enemies - length enemies'')
        state' = case enemies of
            [] -> Win
            _  -> if
                | enemyProjectiles' |> any (within 12 10 ship') -> Lose
                | otherwise -> Play

    in
        {game |
            score <- score',
            duration <- duration',
            ship <- ship',
            projectiles <- projectiles'',
            enemies <- enemies'',
            explosions <- explosions'',
            state <- state',
            enemyProjectiles <- enemyProjectiles'
        }


next : Input -> Game -> Game
next input ({state} as game)=
    case state of
        Play -> stepPlay input game
        Win  -> game
        Lose -> game
