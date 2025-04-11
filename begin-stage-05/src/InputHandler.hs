module InputHandler (handleInput) where

import Control.Monad.State
import Graphics.Gloss.Interface.Pure.Game

import Constants
import GameState

handleInput :: Event -> State GameState ()
handleInput (EventKey (Char 'w') Down _ _) = modify (\ s -> s { paddle1Vel = paddleSpeed })
handleInput (EventKey (Char 's') Down _ _) = modify (\ s -> s { paddle1Vel = -paddleSpeed })
handleInput (EventKey (Char 'w') Up _ _) = modify (\ s -> s { paddle1Vel = 0 })
handleInput (EventKey (Char 's') Up _ _) = modify (\ s -> s { paddle1Vel = 0 })
handleInput (EventKey (SpecialKey KeyUp)   Down _ _) = modify (\ s -> s { paddle2Vel = paddleSpeed })
handleInput (EventKey (SpecialKey KeyDown) Down _ _) = modify (\ s -> s { paddle2Vel = -paddleSpeed })
handleInput (EventKey (SpecialKey KeyUp)   Up _ _) = modify (\ s -> s { paddle2Vel = 0 })
handleInput (EventKey (SpecialKey KeyDown) Up _ _) = modify (\ s -> s { paddle2Vel = 0 })
handleInput _ = return ()