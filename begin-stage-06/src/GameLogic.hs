module GameLogic (gameStep) where

import Control.Monad.State

import Constants
import GameState

gameStep :: Float -> State GameState ()
gameStep _ = do
  state <- get
  if gameMode state == Playing
    then step
    else return ()

step :: State GameState ()
step = do
  checkPoint
  checkEndGame
  gameType <- gets gameType
  if gameType == PvAI
    then updateAI
    else return ()
  updatePaddles
  checkPaddleCollision
  updateBall

updateAI :: State GameState ()
updateAI = do
    game <- get
    let (_, ballY) = ballPos game
        paddleY    = paddle1Y game

        newPaddle1Y
          | ballY > paddleY + 10 = paddleY + speedAI
          | ballY < paddleY - 10 = paddleY - speedAI
          | otherwise = paddleY

    put game { paddle1Y = newPaddle1Y, paddle1Vel = 0 }

updatePaddles :: State GameState ()
updatePaddles = do
  state <- get
  let constrain y = max (-screenHeight / 2 + paddleHeight / 2)
                        (min (screenHeight / 2 - paddleHeight / 2) y)
      newP1Y = constrain (paddle1Y state + paddle1Vel state)
      newP2Y = constrain (paddle2Y state + paddle2Vel state)
  put state { paddle1Y = newP1Y, paddle2Y = newP2Y }

updateBall :: State GameState ()
updateBall = do
  state <- get
  let (bx, by) = ballPos state
      (vx, vy) = ballVel state
      newVy    = if abs by > screenHeight / 2 - ballSize
                   then -vy
                   else vy
      newBx = bx + vx
      newBy = by + newVy
  put state { ballPos = (newBx, newBy), ballVel = (vx, newVy) }

checkEndGame :: State GameState ()
checkEndGame = do
  state <- get
  if score1 state >= pointsToWin
     || score2 state >= pointsToWin
    then put state { gameMode = GameOver}
    else return ()

checkPoint :: State GameState ()
checkPoint = do
  state <- get
  let (newBx, _) = ballPos state
  if newBx > screenWidth / 2 - ballSize
    then put state { ballPos = (0, 0)
                   , ballVel = (ballSpeed, ballSpeed)
                   , score1 = score1 state + 1
                   }
    else if newBx < -screenWidth / 2 + ballSize
      then put state { ballPos = (0, 0)
                     , ballVel = (ballSpeed, ballSpeed)
                     , score2 = score2 state + 1
                     }
      else return ()

checkPaddleCollision :: State GameState ()
checkPaddleCollision = do
  state <- get
  let (bx, by) = ballPos state
      (vx, vy) = ballVel state
  if (bx < -screenWidth / 2 + paddleWidth + 30
        && by > paddle1Y state - paddleHeight / 2
        && by < paddle1Y state + paddleHeight / 2)
      || (bx > screenWidth / 2 - paddleWidth - 30
            && by > paddle2Y state - paddleHeight / 2
            && by < paddle2Y state + paddleHeight / 2)
    then put state { ballVel = (-vx, vy) }
    else return ()
