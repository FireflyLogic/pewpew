module PewPew where

import PewPew.Input (input)
import PewPew.Model (Game, defaultGame)
import PewPew.Level (create)
import PewPew.Step as Step
import PewPew.View as View
import Window

game = { defaultGame | enemies <- create()}

state = foldp Step.next game input
main =  View.display <~ Window.dimensions ~ state
