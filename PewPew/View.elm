module PewPew.View where

import PewPew.Model (..)


textColor = rgb 255 255 255
txt fn message =
    (leftAligned . (typeface ["helvetica", "sans-serif"]) . (Text.color textColor) . fn) (toText message)

displayObj :  Shape -> Object a -> Form
displayObj shape obj =
    move (obj.x,obj.y) (filled white shape)

displayShip: State -> Ship -> Form
displayShip state ship =
    case state of
    Lose -> toForm (fittedImage 30 30 "/assets/explosion.png") |> move (ship.x, ship.y)
    _    -> toForm (fittedImage 40 40 "/assets/ship.png") |> move (ship.x, ship.y)

displayEnemy: Enemy -> Form
displayEnemy enemy =
    toForm (fittedImage 30 30 "/assets/red-2.png")
    |> move (enemy.x, enemy.y)
    |> rotate (degrees 180)

displayExplosion: Explosion -> Form
displayExplosion boom =
    toForm (fittedImage 30 30 "/assets/explosion.png")
    |> move (boom.x, boom.y)
    |> scale (1.2 * boom.time/0.15)




-- display a game state
displayPlay : (Int,Int) -> Game -> Element
displayPlay (w,h) ({state, score, ship, projectiles, enemies, explosions, enemyProjectiles} as game) =
    let objs = [
       filled (rgb 0 0 0) (rect gameWidth gameHeight)
    ] ++
    (map (displayObj (rect 2 6)) projectiles) ++
    (map displayEnemy enemies) ++
    (map displayExplosion explosions) ++
    (map (displayObj (rect 2 6)) enemyProjectiles) ++
    [displayShip state ship]

    in
        layers [
            container w h topLeft <| collage gameWidth gameHeight objs,
            container w 20 topLeft <| (flow down [txt (Text.height 16) (String.append "SCORE: "  (show score))])
        ]

displayGameOver : String -> (Int,Int) -> Game -> Element
displayGameOver message (w,h) game =
    layers [
        displayPlay (w,h) game,
        container w h topLeft <|
            collage gameWidth gameHeight [
                filled (rgba 0 0 0 0.5)  (rect gameWidth gameHeight),
                toForm (txt (Text.height 50) message) |> move (0, gameHeight/2 - 60)
            ]
    ]

display : (Int,Int) -> Game -> Element
display dimensions ({state} as game) =
    case state of
        Play -> displayPlay dimensions game
        Win  -> displayGameOver "You Win!" dimensions game
        Lose -> displayGameOver "Good try!" dimensions game
