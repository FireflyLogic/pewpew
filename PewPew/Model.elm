module PewPew.Model where

import PewPew.Utils (..)

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (gameWidth/2,gameHeight/2)
padding = 4
size = 30.0


data State = Play | Win | Lose

type Object a = { a | x:Float, y:Float, vx:Float, vy:Float }

type Ship = Object {}
type Enemy = Object {}
type Explosion = Object { time: Time }
type Projectile = Object {}
type Collision = (Object, Object)

type Game = {
    --score: Int,
    state: State,
    ship: Ship,
    projectiles: [Projectile],
    enemies: [Enemy],
    explosions: [Explosion]
    -- enemyProjectiles: [Projectile]
    }



enemyVelocity: Int -> Int -> Float
enemyVelocity dir enemiesRemaining =
    (toFloat dir) / (toFloat enemiesRemaining) * 1000

makeEnemy: Int -> Int -> Float -> Enemy
makeEnemy row col vx=
   let y =  200 - (toFloat row * size)
       x = (toFloat col * size) -  300
   in { x=x, y=y, vx=vx, vy=0.0}


defaultGame : Game
defaultGame = {
    --score            = 0,
    state            = Play,
    ship             = { x=-halfWidth, y=20-halfHeight, vx = 0, vy=0},
    projectiles      = [],
    enemies          = [],
    explosions       = []
    -- enemyProjectiles = []
    }
