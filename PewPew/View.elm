module PewPew.View where

import PewPew.Model (..)

txt fn message =
    (leftAligned . (typeface ["helvetica", "sans-serif"]) . (Text.color (rgb 255 255 255)) . fn) (toText message)

displayProjectile : Projectile -> Form
displayProjectile {x,y} =
    move (x,y) (filled white (rect 2 6))


displayShip: State -> Ship -> Form
displayShip state ship =
    case state of
    Lose -> toForm (fittedImage 30 30 "assets/explosion.png") |> move (ship.x, ship.y)
    _    -> toForm (fittedImage 40 40 "assets/ship.png") |> move (ship.x, ship.y)


displayEnemy: Enemy -> Form
displayEnemy enemy =
    toForm (fittedImage 30 30 "assets/red-2.png")
        |> move (enemy.x, enemy.y)
        |> rotate (degrees 180)


displayExplosion: Explosion -> Form
displayExplosion boom =
    toForm (fittedImage 30 30 "assets/explosion.png")
        |> move (boom.x, boom.y)
        |> scale (1.2 * boom.time/0.15)


displayPlay : (Int,Int) -> Game -> Element
displayPlay (w,h) ({state, score, ship, projectiles, enemies, explosions, enemyProjectiles} as game) =
    let objs = [
       filled (rgb 0 0 0) (rect gameWidth gameHeight)
    ] ++
    (map displayProjectile projectiles) ++
    (map displayEnemy enemies) ++
    (map displayExplosion explosions) ++
    (map displayProjectile enemyProjectiles) ++
    [displayShip state ship]

    in
        layers [
            container w h topLeft <| collage gameWidth gameHeight objs,
            container w 20 topLeft <| (flow down [txt (Text.height 16) (String.append "SCORE: "  (show score))])
        ]


tweetLink: Int -> String
tweetLink score =
    let base = "https://twitter.com/intent/tweet?text="
        --HACK: pre-encoded
        text = String.join " " [
            "I%20scored",
            show score,
            "on%20the%20%40FireflyLogic%20404%20game!%20http%3A%2F%2Ffireflylogic.com%2F404%20%23pewpew"
        ]
    in String.append base text


displayGameOver : String -> (Int,Int) -> Game -> Element
displayGameOver message (w,h) ({score} as game) =
    layers [
        displayPlay (w,h) game,
        container w h topLeft <|
            collage gameWidth gameHeight [
                filled (rgba 0 0 0 0.5)  (rect gameWidth gameHeight),
                toForm (txt (Text.height 50) message) |> move (0, 30),
                toForm (txt ((Text.height 20) . (line Under)) "Tweet My Score" |> link (tweetLink score)) |> move (0, -30)
            ]
    ]


display : (Int,Int) -> Game -> Element
display dimensions ({state} as game) =
    case state of
        Play -> displayPlay dimensions game
        Win  -> displayGameOver "You Win!" dimensions game
        Lose -> displayGameOver "Good try!" dimensions game
