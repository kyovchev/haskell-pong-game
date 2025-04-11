module Main (main) where

import Control.Monad.State
import Graphics.Gloss.Interface.IO.Game

import Constants
import GameState
import GameLogic
import InputHandler
import Render


main :: IO ()
main =
  playIO
    (InWindow "Haskell Pong Game" (round screenWidth, round screenHeight) (100, 100))
    black
    30
    initialState
    (pure . render)
    handleInputIO
    (\dt -> pure . execState (gameStep dt))
