import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Game Constants
screenWidth, screenHeight :: Float
screenWidth  = 800
screenHeight = 600

paddleWidth, paddleHeight, paddleSpeed :: Float
paddleWidth  = 20
paddleHeight = 100
paddleSpeed  = 15

ballSize, ballSpeed :: Float
ballSize  = 10
ballSpeed = 5

-- GameState
data GameState = GameState
  { ballPos    :: (Float, Float) -- ball position (x, y)
  , ballVel    :: (Float, Float) -- ball velocity (vx, vy)
  , paddle1Y   :: Float -- paddle 1 position (y)
  , paddle2Y   :: Float -- paddle 2 position (y)
  , paddle1Vel :: Float -- paddle 1 velocity
  , paddle2Vel :: Float -- paddle 2 velocity
  } deriving (Show)

initialState :: GameState
initialState =
  GameState
    { ballPos = (0, 0)
    , ballVel = (ballSpeed, ballSpeed)
    , paddle1Y = 0
    , paddle2Y = 0
    , paddle1Vel = 0
    , paddle2Vel = 0
    }

-- Draw the game
render :: GameState -> Picture
render state = pictures [ball, walls, paddles]
  where
    (bx, by) = ballPos state
    ball = translate bx by $ color red $ circleSolid ballSize
    walls =
      color white $ line [ (-screenWidth / 2, screenHeight / 2)
                         , (screenWidth / 2, screenHeight / 2)
                         ]
    paddle x y =
      translate x y $ color white $ rectangleSolid paddleWidth paddleHeight
    paddles =
      pictures [ paddle (-screenWidth / 2 + 30) (paddle1Y state)
               , paddle (screenWidth / 2 - 30) (paddle2Y state)
               ]

-- Game logic
updatePaddles :: GameState -> GameState
updatePaddles state = state {paddle1Y = newP1Y, paddle2Y = newP2Y}
  where
    constrain y =
      max (-screenHeight / 2 + paddleHeight / 2)
          (min (screenHeight / 2 - paddleHeight / 2) y)
    newP1Y = constrain (paddle1Y state + paddle1Vel state)
    newP2Y = constrain (paddle2Y state + paddle2Vel state)

updateBall :: GameState -> GameState
updateBall state = state {ballPos = (newBx, newBy), ballVel = (vx, newVy)}
  where
    (bx, by) = ballPos state
    (vx, vy) = ballVel state
    newVy =
      if abs by > screenHeight / 2 - ballSize
        then -vy
        else vy
    newBx = bx + vx
    newBy = by + newVy

checkPoint :: GameState -> GameState
checkPoint state =
  if newBx > screenWidth / 2 - ballSize || newBx < -screenWidth / 2 + ballSize
    then state {ballPos = (0, 0), ballVel = (ballSpeed, ballSpeed)}
    else state
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
    then state {ballVel = (-vx, vy)}
    else state
  where
    (bx, by) = ballPos state
    (vx, vy) = ballVel state

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'w') Down _ _) state = state {paddle1Vel = paddleSpeed}
handleInput (EventKey (Char 's') Down _ _) state = state {paddle1Vel = -paddleSpeed}
handleInput (EventKey (Char 'w') Up _ _) state = state {paddle1Vel = 0}
handleInput (EventKey (Char 's') Up _ _) state = state {paddle1Vel = 0}
handleInput (EventKey (SpecialKey KeyUp)   Down _ _) state = state {paddle2Vel = paddleSpeed}
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = state {paddle2Vel = -paddleSpeed}
handleInput (EventKey (SpecialKey KeyUp)   Up _ _) state = state {paddle2Vel = 0}
handleInput (EventKey (SpecialKey KeyDown) Up _ _) state = state {paddle2Vel = 0}
handleInput _ state = state

gameStep :: Float -> GameState -> GameState
gameStep _ state =
  (updateBall . checkPaddleCollision . updatePaddles . checkPoint) state

main :: IO ()
main =
  play
    (InWindow "Haskell Pong Game" (round screenWidth, round screenHeight) (100, 100))
    black
    30
    initialState
    render
    handleInput
    gameStep
