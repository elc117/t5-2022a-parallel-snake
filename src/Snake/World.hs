module Snake.World where
import Snake.Snake ( initSnake, Direction(UP), Food, GridPoint, Snake )
import System.Random ( uniformR, mkStdGen, StdGen )
import Graphics.Gloss.Data.Color

data World = World {snake :: Snake,
                    currentDirection :: Direction,
                    lastMovedDirection :: Direction,
                    timeSinceLastMovement :: Float,
                    totalTime :: Float,
                    randomGenerator :: StdGen,
                    speedBoost :: Bool,
                    gameOver :: Bool,
                    food :: Food,
                    seed :: Int}

initialWorld :: Int -> World
initialWorld seed = World{
    snake = initSnake,
    currentDirection = UP,
    lastMovedDirection = UP,
    timeSinceLastMovement = 0,
    totalTime = 0,
    food = (-1, -1),
    randomGenerator = mkStdGen seed,
    gameOver  = False,
    speedBoost = False,
    seed = seed
  }


gridSize :: (Int, Int)
gridSize = (20, 20)

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