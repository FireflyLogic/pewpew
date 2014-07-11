module PewPew where

import PewPew.Input (..)
import PewPew.Model (Game, defaultGame)
import PewPew.Level (create)
import PewPew.Step (next)
import PewPew.View (display)
import Window

game = { defaultGame | enemies <- create()}

state = foldp next game input
main =  display <~ Window.dimensions ~ state
