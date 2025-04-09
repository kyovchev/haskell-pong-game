import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.State

-- Game Constants
screenWidth, screenHeight :: Float
screenWidth  = 800
screenHeight = 600

paddleWidth, paddleHeight, ballSize :: Float
paddleWidth  = 20
paddleHeight = 100
ballSize     = 10

paddleSpeed, ballSpeed :: Float
paddleSpeed = 15
ballSpeed   = 3


-- GameState
data GameState = GameState
  { ballPos   :: (Float, Float)  -- ball position (x, y)
  , ballVel   :: (Float, Float)  -- ball velocity (vx, vy)
  , paddle1Y  :: Float           -- paddle 1 position (y)
  , paddle2Y  :: Float           -- paddle 2 position (y)
  } deriving Show

initialState :: GameState
initialState = GameState
  { ballPos   = (0, 0)
  , ballVel   = (ballSpeed, ballSpeed)
  , paddle1Y  = 0
  , paddle2Y  = 0
  }


-- Draw the game
render :: GameState -> Picture
render state =
  pictures [ ball, walls, paddles ]
  where
    (bx, by) = ballPos state

    -- Ball
    ball = translate bx by $ color red $ circleSolid ballSize

    -- Walls
    walls = color white $ line [(-screenWidth/2, screenHeight/2), (screenWidth/2, screenHeight/2)]
    
    -- Paddles
    paddle x y = translate x y $ color white $ rectangleSolid paddleWidth paddleHeight
    paddles = pictures [paddle (-screenWidth/2 + 30) (paddle1Y state),
                        paddle (screenWidth/2 - 30) (paddle2Y state)]


-- Game logic
updateBall :: GameState -> GameState
updateBall state = state { ballPos = (newBx, newBy), ballVel = (vx, newVy) }
  where
    (bx, by) = ballPos state
    (vx, vy) = ballVel state
    newVy = if abs by > screenHeight / 2 - ballSize then -vy else vy
    newBx = bx + vx
    newBy = by + newVy

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'w') Down _ _) state =  state { paddle1Y = paddle1Y state + paddleSpeed }
handleInput (EventKey (Char 's') Down _ _) state =  state { paddle1Y = paddle1Y state - paddleSpeed }
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = state { paddle2Y = paddle2Y state + paddleSpeed }
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state =  state { paddle2Y = paddle2Y state - paddleSpeed }
handleInput _ state = state

gameStep :: Float -> GameState -> GameState
gameStep _ state = updateBall state


main :: IO ()
main = play
    (InWindow "Pong" (round screenWidth, round screenHeight) (100, 100))
    black
    30
    initialState
    render
    handleInput
    gameStep
