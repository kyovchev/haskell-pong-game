module Main (main) where

import Graphics.Gloss.Interface.Pure.Game

import Constants
import GameLogic
import GameState
import InputHandler
import Render

main :: IO ()
main =
  play
    (InWindow "Haskell Pong Game" (round screenWidth, round screenHeight) (100, 100))
    black
    30
    initialState
    render
    handleInput
    gameStep
