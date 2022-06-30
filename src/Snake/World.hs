module Snake.World where
import Snake.Snake ( initSnake, Direction(UP), Food, GridPoint, Snake )
import System.Random ( uniformR, mkStdGen, StdGen )

data World = World {snake :: Snake,
                    currentDirection :: Direction,
                    lastMovedDirection :: Direction,
                    timeSinceLastMovement :: Float,
                    totalTime :: Float,
                    randomGenerator :: StdGen,
                    speedBoost :: Bool,
                    gameOver :: Bool,
                    food :: Food}

initialWorld :: World
initialWorld = World{
    snake = initSnake,
    currentDirection = UP,
    lastMovedDirection = UP,
    timeSinceLastMovement = 0,
    totalTime = 0,
    food = (-1, -1),
    randomGenerator = mkStdGen 14,
    gameOver  = False,
    speedBoost = False
  }


gridSize :: (Int, Int)
gridSize = (20, 20)

screenBounds :: (Int, Int)
screenBounds = (640, 640)

tileSize :: (Float, Float)
tileSize = (fromIntegral w / fromIntegral gw, fromIntegral  w / fromIntegral gh)
  where
    (w, h) = screenBounds
    (gw, gh) = gridSize

worldBounds :: (GridPoint, GridPoint)
worldBounds = (leftBottom, rightTop)
  where
    leftBottom = (-halfWidth, -halfheight)
    rightTop = (halfWidth - 1, halfheight - 1)
    halfWidth = gw `div` 2
    halfheight = gh `div` 2
    (gw, gh) = gridSize

invalidRegions :: [GridPoint]
invalidRegions =
  [(x, y) | x <- [(minX-1)..(maxX+1)], y <- [minY-1, maxY+1]] ++
  [(x, y) | y <- [(minY-1)..(maxY+1)], x <- [minX-1, maxX+1]]
  where
      ((minX, minY), (maxX, maxY)) = worldBounds

randomFoodPosition :: Snake -> StdGen -> (GridPoint, StdGen)
randomFoodPosition snake randomGenerator =
  if foodPosition `elem` snake
    then randomFoodPosition snake nextGenerator
    else (foodPosition, nextGenerator)
  where
    foodPosition = (randomX, randomY)
    (randomX, randomGenerator_) = uniformR (minX, maxX) randomGenerator
    (randomY, nextGenerator) = uniformR (minY, maxY) randomGenerator_
    ((minX, minY), (maxX, maxY)) = worldBounds

score :: World -> Float
score world =
  if totalTime world < 1
    then 0
    else (1000 * fromIntegral (length (snake world))) - totalTime world