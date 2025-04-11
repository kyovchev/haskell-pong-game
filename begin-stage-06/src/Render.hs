module Render (render) where

import Graphics.Gloss

import Constants
import GameState


render :: GameState -> Picture
render state = case gameMode state of
    Menu     -> renderMenu
    Playing  -> renderGame state
    GameOver -> renderGameOver state

renderMenu :: Picture
renderMenu = 
  pictures [ title, pvp, pvai ]
  where
    title = translate   (-210) (50) $ color white $ scale 0.3 0.3 $ text "Haskell Pong Game"
    pvp   = translate  (-250) (-80) $ color white $ scale 0.3 0.3 $ text "1 - Player vs Player"
    pvai  = translate (-250) (-150) $ color white $ scale 0.3 0.3 $ text "2 - Player vs AI"

renderGameOver :: GameState -> Picture
renderGameOver state =
  pictures [ winner, spaceToMenu ]
  where
    winner = translate (-150) 0 $ color white $ scale 0.3 0.3
               $ text (if score1 state >= pointsToWin
                         then "Player 1 Wins!"
                         else "Player 2 Wins!")
    spaceToMenu = translate (-230) (-80)
                    $ color white
                    $ scale 0.3 0.3
                    $ text "Press SPACE to Menu"

renderGame :: GameState -> Picture
renderGame state =
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