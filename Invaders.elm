module GameSkeleton where

import Keyboard
import Text
import Window
import List

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type Input = { space:Bool, direction:Int, delta:Time }

delta = inSeconds <~ fps 60

input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .x Keyboard.arrows
                               ~ delta)



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

data State = Start | Play | End

type Object a = { a | x:Float, y:Float, vx:Float, vy:Float }

type Ship = Object {}
-- type Enemy = Object { hits:Int }
type Projectile = Object {}

type Game = {
    score: Int,
    state: State,
    ship: Ship,
    projectiles: [Projectile]
    -- enemies: [Enemy],
    -- enemyProjectiles: [Projectile]
    }

-- enemy : Int ->  Player
-- enemy x hits = { x=x, y=0, vx=0, vy=0, hits=hits }

-- with_index list = zip [0..(length list) ] list

defaultGame : Game
defaultGame = {
    score            = 0,
    state            = Start,
    ship             = { x=-halfWidth, y=20-halfHeight, vx = 0, vy=0},
    projectiles      = []
    -- enemies          = [],
    -- enemyProjectiles = []
    }



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepObj : Time -> Object a -> Object a
stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx * t
          , y <- y + vy * t }

stepShip : Time -> Int -> Ship -> Ship
stepShip t dir ship =
    let shipWidth = 40
        ship' = stepObj t { ship | vx <- toFloat dir * 600 }
        x'      = clamp (shipWidth/2-halfWidth) (halfWidth-shipWidth/2) ship'.x
  in
      { ship' | x <- x'}

isOnScreen projectile = projectile.y < halfHeight

stepProjectiles : Time -> Bool -> Float -> [Projectile] -> [Projectile]
stepProjectiles t shooting origin projectiles =
    let maxProjectiles = 10
        projectileDistance = 60
        projectiles' =  projectiles |> map (stepObj t) |> filter isOnScreen
        canShoot =
            length projectiles' < 20
             && case projectiles' of
                 head::tail -> head.y  >  projectileDistance - halfHeight
                 [] -> True
    in
        if
            | shooting && canShoot -> { x=origin, y=20-halfHeight, vx = 0, vy=200} :: projectiles'
            | otherwise -> projectiles'

stepGame : Input -> Game -> Game
stepGame {space,direction,delta}
         ({score, state, ship, projectiles} as game)=
    let ship' = stepShip delta direction ship
        projectiles' = stepProjectiles delta space ship.x projectiles
    in
        {game |
            ship <- ship',
            projectiles <- projectiles'
        }




{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

-- display : (Int,Int) -> Game -> Element
-- display (w,h) gameState = asText gameState

-- helper values
starField = rgb 0 0 0

-- shared function for rendering objects
displayObj :  Shape -> Object a -> Form
displayObj shape obj =
    move (obj.x,obj.y) (filled white shape)

-- display a game state
display : (Int,Int) -> Game -> Element
display (w,h) ({score, state, ship, projectiles} as game) =
    let lol = [
       filled starField   (rect gameWidth gameHeight),
       toForm (fittedImage 40 40 "/assets/ship.png") |> move (ship.x, ship.y)
    ] ++ (map (displayObj (rect 2 6)) projectiles)

    in
        container w h middle <|
            collage gameWidth gameHeight lol




{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}



game = foldp stepGame defaultGame input

main = lift2 display Window.dimensions game
