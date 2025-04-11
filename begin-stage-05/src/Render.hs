module Render (render) where

import Graphics.Gloss

import Constants
import GameState

render :: GameState -> Picture
render state =
  pictures [ ball, walls, paddles, scoreText ]
  where
    (bx, by) = ballPos state
    ball = translate bx by $ color red $ circleSolid ballSize
    walls = color white $ line [ (-screenWidth / 2, screenHeight / 2)
                               , (screenWidth / 2, screenHeight / 2)
                               ]
    paddle x y = translate x y $ color white $ rectangleSolid paddleWidth paddleHeight
    paddles = pictures [ paddle (-screenWidth / 2 + 30) (paddle1Y state)
                       , paddle (screenWidth / 2 - 30) (paddle2Y state)
                       ]
    scoreText =
      translate (-50) (screenHeight / 2 - 50)
        $ scale 0.3 0.3
        $ color white
        $ text (show (score1 state) ++ " : " ++ show (score2 state))