module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , paddleWidth :: Float
  , paddleHeight :: Float
  , player1Position :: Position
  , player2Position :: Position
  , p1Movement :: Float
  , p2Movement :: Float
  , paused :: Bool
  } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (80, 60)
  , paddleWidth = 24
  , paddleHeight = 80
  , player1Position = (-120,100)
  , player2Position = (120,0)
  , p1Movement = 0
  , p2Movement = 0
  , paused = False
  }


type Radius = Float 
type Position = (Float, Float)

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle rose $ player1Position game,
            mkPaddle orange $ player2Position game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Position -> Picture
    mkPaddle col (x, y) = pictures
      [ translate x y $ color col $ rectangleSolid width height
      , translate x y $ color paddleColor $ rectangleSolid (width - 6) (height - 6)
      ]

    width = paddleWidth game
    height = paddleHeight game
    paddleColor = light (light blue)

handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'n') Up _ _) game = game { ballLoc = (0,0) }
handleKeys (EventKey (Char 'p') Down _ _) game = game { paused = not $ paused game } 
handleKeys (EventKey (Char 'w') Down _ _) game = game { p1Movement = playerSpeed }
handleKeys (EventKey (Char 'w') Up _ _) game = game { p1Movement = 0 }
handleKeys (EventKey (Char 's') Down _ _) game = game { p1Movement = -playerSpeed }
handleKeys (EventKey (Char 's') Up _ _) game = game { p1Movement = 0 }
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { p2Movement = playerSpeed } 
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { p2Movement = 0 } 
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { p2Movement = -playerSpeed }
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game { p2Movement = 0 }
handleKeys _ game = game

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
-- paddleBounce :: PongGame -> PongGame
-- paddleBounce game = game { ballVel = (vx', vy') }
--     where
--         radius = 10
--         (vx, vy) = ballVel game

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx', vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius then -vy else vy
    vx' = if paddleCollision (ballLoc game) (player1Position game) (player2Position game) radius (paddleHeight game) (paddleWidth game)
          then
            -vx
          else
            vx

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

movePaddles :: PongGame -> PongGame
movePaddles = updateP1Position . updateP2Position

updateP1Position :: PongGame -> PongGame
updateP1Position game = game {player1Position = (x, limitMovement y' height (paddleHeight game))}
                        where
                          x = fst $ player1Position game
                          y' = p1Movement game + snd (player1Position game)

updateP2Position :: PongGame -> PongGame
updateP2Position game = game {player2Position = (x, limitMovement y' height (paddleHeight game))}
                        where
                          x = fst $ player2Position game
                          y' = p2Movement game + snd (player2Position game)

limitMovement :: Float -> Int -> Float -> Float
limitMovement move height pHeight
        | move > upperLimit = upperLimit
        | move < lowerLimit = lowerLimit 
        | otherwise = move
        where
          upperLimit = fHeight/2 - pHeight/2
          lowerLimit = pHeight/2 - fHeight/2 
          fHeight = fromIntegral height :: Float

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds game = if not (paused game) then (movePaddles . wallBounce . moveBall seconds) game else game


-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2


paddleCollision :: Position -> Position -> Position -> Radius -> Float -> Float -> Bool 
paddleCollision (bx,by) (p1x,p1y) (p2x,p2y) radius pHeight pWidth = leftPaddleCollision || rightPaddleCollision
    where
        leftPaddleCollision = bx - radius <= p1x + pWidth/2 && by - radius >= p1y - (pHeight/2) && by + radius <= p1y + (pHeight/2)
        rightPaddleCollision = bx + radius  >= p2x - pWidth/2 && by + radius >= p2y - (pHeight/2) && by + radius <= p2y + (pHeight/2)

width, height, offset :: Int
width = 300
height = 300
offset = 100

playerSpeed :: Float
playerSpeed = 5

fps :: Int
fps = 60

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = play window background fps initialState render handleKeys update
