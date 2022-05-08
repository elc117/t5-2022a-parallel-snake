module Game (run) where

import Snake.Snake
import Snake.World
import Text.Printf
import Graphics.UI.Fungen
--import Graphics.Rendering.OpenGL (GLdouble)
import Paths_snake_haskell (getDataFileName)

type GLdouble = Double

run :: IO ()
run = main

data GameAttribute = GA Int Int Snake
data ObjectAttribute = NoObjectAttribute | Tail Int

type WormsAction a = IOGame GameAttribute ObjectAttribute TileAttribute a
type WormsObject = GameObject ObjectAttribute
type WormsTile = Tile TileAttribute
type WormsMap = TileMatrix TileAttribute

speedMod :: GLdouble
speedMod = 30.0

maxFood, initTailSize, defaultTimer :: Int
maxFood = 10
initTailSize = 2
defaultTimer = 10

main :: IO ()
main = do
  let winConfig = ((200,100),(780,600),"Snake in haskell")

      gameMap = multiMap [(tileMap map1 tileSize tileSize)] 0

      gameAttribute = GA defaultTimer maxFood initSnake

      groups = [(objectGroup "snake"     [createHead]),
                (objectGroup "food"     [createFood])]

      input = [
               (SpecialKey KeyLeft,  Press, turnLeft ),
               (SpecialKey KeyRight, Press, turnRight),
               (SpecialKey KeyUp,    Press, turnUp   ),
               (SpecialKey KeyDown,  Press, turnDown )
              ,(Char 'q',            Press, \_ _ -> funExit)
              ]

  funInit winConfig gameMap groups (LevelStart 1) gameAttribute input gameCycle (Timer 150) bmpList'

createHead :: WormsObject
createHead = let pic = Tex (tileSize,tileSize) 5
             in object "head" pic True initPos (0,speedMod) NoObjectAttribute

createFood :: WormsObject
createFood = let pic = Tex (tileSize,tileSize) 9
             in object "food" pic True (0,0) (0,0) NoObjectAttribute

turnLeft :: Modifiers -> Position -> WormsAction()
turnLeft _ _ = do
  snakeHead <- findObject "head" "head"
  setObjectPosition  snakeHead
  setObjectCurrentPicture 8 snakeHead
  setObjectSpeed (-speedMod,0) snakeHead

turnRight :: Modifiers -> Position -> WormsAction()
turnRight _ _ = do
  snakeHead <- findObject "head" "head"
  setObjectCurrentPicture 7 snakeHead
  setObjectSpeed (speedMod,0) snakeHead

turnUp :: Modifiers -> Position -> WormsAction()
turnUp _ _ = do
  snakeHead <- findObject "head" "head"
  setObjectCurrentPicture 5 snakeHead
  setObjectSpeed (0,speedMod) snakeHead

turnDown :: Modifiers -> Position -> WormsAction ()
turnDown _ _ = do
  snakeHead <- findObject "head" "head"
  setObjectCurrentPicture 6 snakeHead
  setObjectSpeed (0,-speedMod) snakeHead

gameCycle :: WormsAction()
gameCycle = do
    (GA timer remainingFood snake) <- getGameAttribute
    if (remainingFood == 0) -- advance level!
        then  (do   setGameState (LevelStart (n + 1))
                    disableGameFlags
                    setGameAttribute (GA timer maxFood initTailSize initPos score))
        else if (timer == 0) -- put a new food in the map
                then (do    food <- findObject "food" "food"
                            newPos <- createNewFoodPosition
                            setObjectPosition newPos food
                            newFood <- findObject "food" "food"
                            setObjectAsleep False newFood
                            setGameAttribute (GA (-1) remainingFood tailSize previousHeadPos score)
                            snakeHead <- findObject "head" "head"
                            checkSnakeCollision snakeHead
                            snakeHeadPosition <- getObjectPosition snakeHead
                            moveTail snakeHeadPosition)
                else if (timer > 0) -- there is no food in the map, so decrease the food timer
                    then (do    setGameAttribute (GA (timer - 1) remainingFood tailSize previousHeadPos score)
                                snakeHead <- findObject "head" "head"
                                checkSnakeCollision snakeHead
                                snakeHeadPosition <- getObjectPosition snakeHead
                                moveTail snakeHeadPosition)
                    else (do -- there is a food in the map
                            food <- findObject "food" "food"
                            snakeHead <- findObject "head" "head"
                            col <- objectsCollision snakeHead food
                            if col
                                then (do    snakeHeadPosition <- getObjectPosition snakeHead
                                            setGameAttribute (GA defaultTimer (remainingFood-1) (tailSize + 1) snakeHeadPosition (score + 1))
                                            addTail previousHeadPos
                                            setObjectAsleep True food)
                            else (do    checkSnakeCollision snakeHead
                                        snakeHeadPosition <- getObjectPosition snakeHead
                                        moveTail snakeHeadPosition))
    showScore

showScore :: WormsAction ()
showScore = do
  (GA _ remainingFood snake) <- getGameAttribute
  printOnScreen (printf "Score: %d    Food remaining: %d" (length snake) remainingFood) TimesRoman24 (40,8) 1.0 1.0 1.0
  showFPS TimesRoman24 (780-60,8) 1.0 0.0 0.0

checkSnakeCollision :: WormsObject -> WormsAction ()
checkSnakeCollision snakeHead = error "Not implemented"
createNewFoodPosition :: WormsAction (GLdouble,GLdouble)
createNewFoodPosition = error "Not implemented"
