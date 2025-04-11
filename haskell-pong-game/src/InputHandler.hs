module InputHandler (handleInputIO) where

import Control.Monad.State
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)

import Constants
import GameState

handleInputIO :: Event -> GameState -> IO GameState
handleInputIO (EventKey (SpecialKey KeyEsc) Up _ _) state =
  case gameMode state of
    Menu    -> exitSuccess
    Playing -> return state { gameMode = Paused }
    _       -> return state
handleInputIO event state = do
  let newState = execState (handleInput event) state
  return newState

handleInput :: Event -> State GameState ()
handleInput event = do
    state <- get
    case gameMode state of
      Menu     -> handleMenuInput event
      Playing  -> handleGameInput event
      Paused   -> handlePausedInput event
      GameOver -> handleGameOverInput event

handleMenuInput :: Event -> State GameState ()
handleMenuInput (EventKey (Char '1') Down _ _) = put initialState { gameMode = Playing, gameType = PvP }
handleMenuInput (EventKey (Char '2') Down _ _) = put initialState { gameMode = Playing, gameType = PvAI }
handleMenuInput _ = return ()

handlePausedInput :: Event -> State GameState ()
handlePausedInput (EventKey (Char '1') Down _ _) = modify (\ s -> s { gameMode = Playing })
handlePausedInput (EventKey (Char '2') Down _ _) = put initialState { gameMode = Menu }
handlePausedInput _ = return ()

handleGameOverInput :: Event -> State GameState ()
handleGameOverInput (EventKey (SpecialKey KeySpace) Up _ _) = put initialState
handleGameOverInput _ = return ()

handleGameInput :: Event -> State GameState ()
handleGameInput (EventKey (Char 'w') Down _ _) = modify (\ s -> s { paddle1Vel = paddleSpeed })
handleGameInput (EventKey (Char 's') Down _ _) = modify (\ s -> s { paddle1Vel = -paddleSpeed })
handleGameInput (EventKey (Char 'w') Up _ _) = modify (\ s -> s { paddle1Vel = 0 })
handleGameInput (EventKey (Char 's') Up _ _) = modify (\ s -> s { paddle1Vel = 0 })
handleGameInput (EventKey (SpecialKey KeyUp)   Down _ _) = modify (\ s -> s { paddle2Vel = paddleSpeed })
handleGameInput (EventKey (SpecialKey KeyDown) Down _ _) = modify (\ s -> s { paddle2Vel = -paddleSpeed })
handleGameInput (EventKey (SpecialKey KeyUp)   Up _ _) = modify (\ s -> s { paddle2Vel = 0 })
handleGameInput (EventKey (SpecialKey KeyDown) Up _ _) = modify (\ s -> s { paddle2Vel = 0 })
handleGameInput _ = return ()