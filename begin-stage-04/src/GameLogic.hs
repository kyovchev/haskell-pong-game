module GameLogic (gameStep) where

import Constants
import GameState

updatePaddles :: GameState -> GameState
updatePaddles state = state { paddle1Y = newP1Y, paddle2Y = newP2Y }
  where
    constrain y = max (-screenHeight / 2 + paddleHeight / 2)
                      (min (screenHeight / 2 - paddleHeight / 2) y)
    newP1Y = constrain (paddle1Y state + paddle1Vel state)
    newP2Y = constrain (paddle2Y state + paddle2Vel state)

updateBall :: GameState -> GameState
updateBall state = state { ballPos = (newBx, newBy), ballVel = (vx, newVy) }
  where
    (bx, by) = ballPos state
    (vx, vy) = ballVel state
    newVy = if abs by > screenHeight / 2 - ballSize then -vy else vy
    newBx = bx + vx
    newBy = by + newVy

checkPoint :: GameState -> GameState
checkPoint state
  | newBx > screenWidth / 2 - ballSize = state { ballPos = (0, 0)
                                               , ballVel = (ballSpeed, ballSpeed)
                                               , score1 = score1 state + 1
                                               }
  | newBx < -screenWidth / 2 + ballSize = state { ballPos = (0, 0)
                                                , ballVel = (ballSpeed, ballSpeed)
                                                , score2 = score2 state + 1
                                                }
  | otherwise = state
  where
    (newBx, _) = ballPos state

checkPaddleCollision :: GameState -> GameState
checkPaddleCollision state =
  if (bx < -screenWidth / 2 + paddleWidth + 30
        && by > paddle1Y state - paddleHeight / 2
        && by < paddle1Y state + paddleHeight / 2)
      || (bx > screenWidth / 2 - paddleWidth - 30
            && by > paddle2Y state - paddleHeight / 2
            && by < paddle2Y state + paddleHeight / 2)
    then state { ballVel = (-vx, vy) }
    else state
  where
    (bx, by) = ballPos state
    (vx, vy) = ballVel state

gameStep :: Float -> GameState -> GameState
gameStep _ = updateBall . checkPaddleCollision . updatePaddles . checkPoint