module GameState where

import Constants

data GameState = GameState
  { ballPos   :: (Float, Float)  -- ball position (x, y)
  , ballVel   :: (Float, Float)  -- ball velocity (vx, vy)
  , paddle1Y  :: Float           -- paddle 1 position (y)
  , paddle2Y  :: Float           -- paddle 2 position (y)
  , paddle1Vel :: Float          -- paddle 1 velocity
  , paddle2Vel :: Float          -- paddle 2 velocity
  , score1   :: Int              -- player 1 score
  , score2   :: Int              -- player 2 score
  } deriving Show

initialState :: GameState
initialState = GameState
  { ballPos   = (0, 0)
  , ballVel   = (ballSpeed, ballSpeed)
  , paddle1Y  = 0
  , paddle2Y  = 0
  , paddle1Vel = 0
  , paddle2Vel = 0
  , score1   = 0
  , score2   = 0
  }