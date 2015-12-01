module PewPew.Model where

import PewPew.Utils as Utils
import Time exposing (Time)

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (gameWidth/2,gameHeight/2)

type State = Play | Win | Lose

type alias Object a = { a | x:Float, y:Float, vx:Float, vy:Float }

type alias Ship = Object {}

type alias Enemy = Object {
    lastFired: Time
}

type alias Explosion = Object {
    time: Time
}

type alias Projectile = Object {}

type alias Game = {
    score: Int,
    duration: Time,
    state: State,
    ship: Ship,
    projectiles: List Projectile,
    enemies: List Enemy,
    explosions: List Explosion,
    enemyProjectiles: List Projectile
}

enemyVelocity: Int -> Int -> Float
enemyVelocity dir enemiesRemaining =
    let velocity = 200 - Utils.cubicEasing 34 0 175 enemiesRemaining
    in velocity * (toFloat dir)


makeEnemy: Int -> Int -> Float -> Enemy
makeEnemy row col vx=
   let enemySize = 30
       y =  halfHeight - (toFloat row * enemySize) - 20.0
       x = (toFloat col * enemySize) - halfWidth

   in { x=x, y=y, vx=vx, vy=0.0, lastFired=0 }


defaultGame : Game
defaultGame = {
    score            = 0,
    duration         = 0,
    state            = Play,
    ship             = { x=-halfWidth, y=20-halfHeight, vx = 0, vy=0 },
    projectiles      = [],
    enemies          = [],
    explosions       = [],
    enemyProjectiles = []}
