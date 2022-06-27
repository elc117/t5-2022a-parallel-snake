module Game (run) where

import Snake.Snake
import Text.Printf
import Paths_snake_haskell (getDataFileName)
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.Map

run :: IO ()
run = main

data World = World {snake :: Snake,
                    currentDirection :: Direction,
                    timeSinceLastMovement :: Float,
                    randomGenerator :: StdGen,
                    food :: Food}

main :: IO()
main =
  let world = World{snake = initSnake, currentDirection = UP, timeSinceLastMovement = 0, food = (-1, -1), randomGenerator = mkStdGen 27} in
  play FullScreen white 60 world worldToPicture handleInputs mainLoop

timePerMovement :: Float
timePerMovement = 0.25

mainLoop :: Float -> World -> World
mainLoop deltaTime world = world{snake = nextSnake, currentDirection = nextDirection, timeSinceLastMovement = nextTimeSinceLastMovement, food = nextFood, randomGenerator = nextGenerator}
  where
    moveNow = timeSinceLastMovement world > timePerMovement
    nextDirection = currentDirection world
    nextSnake =
      if moveNow
        then move (snake world) nextDirection nextFood
        else snake world
    (randomX, g) = uniformR (-10, 10) (randomGenerator world)
    (randomY, nextGenerator) = uniformR (-10, 10) g
    nextFood =
      if head (snake world) == food world
        then (randomX, randomY)
        else food world
    nextTimeSinceLastMovement =
      if moveNow
        then 0
        else timeSinceLastMovement world + deltaTime

inputs = fromList [
  ('w', \world -> if currentDirection world == DOWN then world else world{currentDirection = UP}),
  ('a', \world -> if currentDirection world == RIGHT then world else world{currentDirection = LEFT}),
  ('s', \world -> if currentDirection world == UP then world else world{currentDirection = DOWN}),
  ('d', \world -> if currentDirection world == LEFT then world else world{currentDirection = RIGHT})
  ]

handleInputs :: Event -> World -> World
handleInputs (EventKey (Char c) Down _ _) world =
  case maybeAction of
    Just action -> action world
    Nothing -> world
  where
    maybeAction = Data.Map.lookup c inputs
handleInputs _ world = world


worldToPicture :: World -> Picture
worldToPicture world = Pictures ([time] ++ snakePicture ++ [foodPicture])
  where
    time = Text (show $ timeSinceLastMovement world)
    snakePicture = Prelude.map snakePointToPicture (snake world)
    foodPicture = foodToPicture (food world)

tileSize :: (Float, Float)
tileSize = (10, 10)

snakePointToPicture :: GridPoint -> Picture
snakePointToPicture snakePoint = Color black (gridPointToPicture snakePoint)

foodToPicture :: GridPoint -> Picture
foodToPicture food = Color red (gridPointToPicture food)

gridPointToPicture :: GridPoint -> Picture
gridPointToPicture  (x, y) =
  Polygon [tl, tr, br, bl]
  where
    (tilex, tiley) = tileSize
    (startx, starty) = (fromIntegral x * tilex, fromIntegral y * tiley)
    tl = (startx, starty)
    tr = (startx + tilex, starty)
    br = (startx + tilex, starty + tiley)
    bl = (startx, starty + tiley)
