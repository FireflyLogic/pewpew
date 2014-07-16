module PewPew.View where

import PewPew.Model (..)

-- helper values
starField = rgb 0 0 0
textColor = rgb 255 255 255
txt f = leftAligned . f . monospace . Text.color textColor . toText

-- shared function for rendering objects
displayObj :  Shape -> Object a -> Form
displayObj shape obj =
    move (obj.x,obj.y) (filled white shape)

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
displayPlay (w,h) ({ship, projectiles, enemies, explosions, enemyProjectiles} as game) =
    let objs = [
       filled starField   (rect gameWidth gameHeight),
       toForm (fittedImage 40 40 "/assets/ship.png") |> move (ship.x, ship.y)
    ] ++
    (map (displayObj (rect 2 6)) projectiles) ++
    (map displayEnemy enemies) ++
    (map displayExplosion explosions) ++
    (map (displayObj (rect 2 6)) enemyProjectiles)

    in
        layers [
        container w h midTop <| asText game.score,
        container w h middle <|
            collage gameWidth gameHeight objs
            ]

displayWin : (Int,Int) -> Game -> Element
displayWin (w,h) game =
    container w h middle <|
        collage gameWidth gameHeight [
           filled starField   (rect gameWidth gameHeight),
           (toForm (txt (Text.height 50) <| show "You WIN!")) |> move (0, gameHeight/2 - 40)]

displayLose : (Int,Int) -> Game -> Element
displayLose (w,h) game =
    container w h middle <|
        collage gameWidth gameHeight [
           filled starField   (rect gameWidth gameHeight),
           (toForm (txt (Text.height 50) <| show "You Lost!")) |> move (0, gameHeight/2 - 40)]

display : (Int,Int) -> Game -> Element
display dimensions ({state} as game) =
    case state of
        Play -> displayPlay dimensions game
        Win  -> displayWin dimensions game
        Lose -> displayLose dimensions game
