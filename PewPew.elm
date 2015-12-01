module PewPew where

import PewPew.Input exposing (input)
import PewPew.Model exposing (defaultGame)
import PewPew.Level as Level
import PewPew.Step as Step
import PewPew.View as View
import Signal
import Window

game  = { defaultGame | enemies = Level.create() }
state = Signal.foldp Step.next game input
main  = Signal.map2 View.display Window.dimensions state
