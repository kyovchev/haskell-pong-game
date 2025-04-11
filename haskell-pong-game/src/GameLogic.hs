module GameLogic (gameStep) where

import Control.Monad (when)
import Control.Monad.State

import Constants
import GameState

gameStep :: Float -> State GameState ()
gameStep _ = do
  state <- get
  when (gameMode state == Playing) $ step

step :: State GameState ()
step = do
  checkPoint
  checkEndGame
  gameType <- gets gameType
  when (gameType == PvAI) $ updateAI
  updatePaddles
  checkPaddleCollision
  updateBall

updateAI :: State GameState ()
updateAI = do
  game <- get
  let (_, ballY) = ballPos game
      paddleY = paddle1Y game
      newPaddle1Y
        | ballY > paddleY + 10 = paddleY + speedAI
        | ballY < paddleY - 10 = paddleY - speedAI
        | otherwise = paddleY
  put game {paddle1Y = newPaddle1Y, paddle1Vel = 0}

updatePaddles :: State GameState ()
updatePaddles = do
  state <- get
  let constrain y =
        max
          (-screenHeight / 2 + paddleHeight / 2)
          (min (screenHeight / 2 - paddleHeight / 2) y)
      newP1Y = constrain (paddle1Y state + paddle1Vel state)
      newP2Y = constrain (paddle2Y state + paddle2Vel state)
  put state {paddle1Y = newP1Y, paddle2Y = newP2Y}

updateBall :: State GameState ()
updateBall = do
  state <- get
  let (bx, by) = ballPos state
      (vx, vy) = ballVel state
      newVy =
        if abs by > screenHeight / 2 - ballSize
          then -vy
          else vy
      newBx = bx + vx
      newBy = by + newVy
  put state {ballPos = (newBx, newBy), ballVel = (vx, newVy)}

checkEndGame :: State GameState ()
checkEndGame = do
  state <- get
  when (score1 state >= pointsToWin || score2 state >= pointsToWin)
    $ put state {gameMode = GameOver}

resetBall :: GameState -> GameState
resetBall state = state {ballPos = (0, 0), ballVel = (ballSpeed, ballSpeed)}

checkPoint :: State GameState ()
checkPoint = do
  state <- get
  let (newBx, _) = ballPos state
  when (newBx > screenWidth / 2 - ballSize)
    $ put (resetBall state) {score1 = score1 state + 1}
  when (newBx < -screenWidth / 2 + ballSize)
    $ put (resetBall state) {score2 = score2 state + 1}

checkPaddleCollision :: State GameState ()
checkPaddleCollision = do
  state <- get
  let (bx, by) = ballPos state
      (vx, vy) = ballVel state
  when
    ((bx < -screenWidth / 2 + paddleWidth + 30
        && by > paddle1Y state - paddleHeight / 2
        && by < paddle1Y state + paddleHeight / 2)
       || (bx > screenWidth / 2 - paddleWidth - 30
             && by > paddle2Y state - paddleHeight / 2
             && by < paddle2Y state + paddleHeight / 2))
    $ put state {ballVel = (-vx, vy)}
