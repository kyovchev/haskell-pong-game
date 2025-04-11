module InputHandler (handleInput) where

import Graphics.Gloss.Interface.Pure.Game

import Constants
import GameState


handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'w') Down _ _) state = state { paddle1Vel = paddleSpeed }
handleInput (EventKey (Char 's') Down _ _) state = state { paddle1Vel = -paddleSpeed }
handleInput (EventKey (Char 'w') Up _ _) state = state { paddle1Vel = 0 }
handleInput (EventKey (Char 's') Up _ _) state = state { paddle1Vel = 0 }
handleInput (EventKey (SpecialKey KeyUp)   Down _ _) state = state { paddle2Vel = paddleSpeed }
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = state { paddle2Vel = -paddleSpeed }
handleInput (EventKey (SpecialKey KeyUp)   Up _ _) state = state { paddle2Vel = 0 }
handleInput (EventKey (SpecialKey KeyDown) Up _ _) state = state { paddle2Vel = 0 }
handleInput _ state = state