module Main (main) where

import Control.Monad.State
import Graphics.Gloss.Interface.Pure.Game

import Constants
import GameState
import GameLogic
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
    (\event -> execState (handleInput event))
    (\dt -> execState (gameStep dt))
