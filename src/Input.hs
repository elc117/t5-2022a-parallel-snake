module Input where

import Snake.Snake ( Direction(RIGHT, UP, DOWN, LEFT) )
import Snake.World
import Graphics.Gloss.Interface.Pure.Game
import Data.Map ( fromList, lookup )
import Snake.MultiWorld (MultiWorld)
import Control.Parallel.Strategies (parMap, rpar)
import System.Random (uniformR)

inputs = fromList [
  ('w', \world -> if lastMovedDirection world == DOWN then world else world{currentDirection = UP}),
  ('a', \world -> if lastMovedDirection world == RIGHT then world else world{currentDirection = LEFT}),
  ('s', \world -> if lastMovedDirection world == UP then world else world{currentDirection = DOWN}),
  ('d', \world -> if lastMovedDirection world == LEFT then world else world{currentDirection = RIGHT}),
  ('r', \world -> if gameOver world then initialWorld (fst (uniformR (14, 27) (randomGenerator world))) else world)
  ]

handleInputs :: Event -> MultiWorld -> MultiWorld
handleInputs (EventKey (SpecialKey KeySpace) state _ _) multiWorld =
  parMap rpar (\world -> world{speedBoost = state == Down}) multiWorld

handleInputs (EventKey (Char c) Down _ _) multiWorld =
  case maybeAction of
    Just action -> parMap rpar action multiWorld
    Nothing -> multiWorld
  where
    maybeAction = Data.Map.lookup c inputs
handleInputs _ multiWorld = multiWorld