module Game (run) where

import Snake.Snake
import Snake.World
import Input
import Graphics
import Text.Printf
import Graphics.Gloss.Interface.Pure.Game
import System.Random

run :: IO ()
run = main

main :: IO()
main =
  let world = initialWorld in play screen white 60 world worldToPicture handleInputs mainLoop

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
        then timeSinceLastMovement world > (timePerMovement * speedBoostModifier)
        else timeSinceLastMovement world > timePerMovement