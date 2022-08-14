module Game (run) where

import Snake.Snake ( isDead, move, speed, speedBoostModifier )
import Snake.World
import Input ( handleInputs )
import Graphics ( screen, worldToPicture, multiWorldToPicture )
import Graphics.Gloss.Interface.Pure.Game ( white, play )
import Graphics.Gloss.Data.Display (Display(FullScreen))
import Snake.MultiWorld (MultiWorld, initialMultiWorld)
import Control.Parallel.Strategies

run :: IO ()
run =
  let world = initialMultiWorld in play screen white 60 world multiWorldToPicture handleInputs multiWorldMainLoop

multiWorldMainLoop :: Float -> MultiWorld -> MultiWorld
multiWorldMainLoop deltaTime multiWorld = parMap rpar (\world -> mainLoop deltaTime world) multiWorld

mainLoop :: Float -> World -> World
mainLoop deltaTime world =
  if gameOver world
    then gameOverState deltaTime world
    else runningState deltaTime world


gameOverState :: Float -> World -> World
gameOverState deltaTime world = world

runningState :: Float -> World -> World
runningState deltaTime world = 
  if nextGameOver
    then world{
      gameOver = True
    }
    else world{
      snake = nextSnake,
      timeSinceLastMovement = nextTimeSinceLastMovement,
      food = nextFood,
      randomGenerator = nextGenerator,
      lastMovedDirection = nextLastMovedDirection,
      totalTime = totalTime world + deltaTime
    }
  where
    moveNow = shouldMoveNow world
    nextSnake =
      if moveNow
        then move (snake world) (currentDirection world) nextFood
        else snake world
    nextGameOver = isDead nextSnake invalidRegions
    (nextFood, nextGenerator) =
      if head (snake world) == food world
        then randomFoodPosition (snake world) (randomGenerator world)
        else (food world, randomGenerator world)
    nextTimeSinceLastMovement =
      if moveNow
        then 0
        else timeSinceLastMovement world + deltaTime
    nextLastMovedDirection =
      if moveNow
        then currentDirection world
        else lastMovedDirection world
  
shouldMoveNow :: World -> Bool
shouldMoveNow world = 
  if speedBoost world
    then timeSinceLastMovement world > (timePerMovement / speedBoostModifier)
    else timeSinceLastMovement world > timePerMovement
  where
    timePerMovement = 1 / actualSpeed
    actualSpeed = speed + fromIntegral (length (snake world) `div` 3)