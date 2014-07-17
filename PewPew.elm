module PewPew where

import PewPew.Input (input)
import PewPew.Model (defaultGame)
import PewPew.Level as Level
import PewPew.Step as Step
import PewPew.View as View
import Window

game  = { defaultGame | enemies <- Level.create()}
state = foldp Step.next game input
main  = View.display <~ Window.dimensions ~ state
