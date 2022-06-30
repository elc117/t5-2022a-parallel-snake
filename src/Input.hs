module Input where

import Snake.Snake ( Direction(RIGHT, UP, DOWN, LEFT) )
import Snake.World
import Graphics.Gloss.Interface.Pure.Game
import Data.Map ( fromList, lookup )

inputs = fromList [
  ('w', \world -> if lastMovedDirection world == DOWN then world else world{currentDirection = UP}),
  ('a', \world -> if lastMovedDirection world == RIGHT then world else world{currentDirection = LEFT}),
  ('s', \world -> if lastMovedDirection world == UP then world else world{currentDirection = DOWN}),
  ('d', \world -> if lastMovedDirection world == LEFT then world else world{currentDirection = RIGHT}),
  ('r', \world -> if gameOver world then initialWorld else world)
  ]

handleInputs :: Event -> World -> World
handleInputs (EventKey (SpecialKey KeySpace) state _ _) world = world{speedBoost = state == Down}
handleInputs (EventKey (Char c) Down _ _) world =
  case maybeAction of
    Just action -> action world
    Nothing -> world
  where
    maybeAction = Data.Map.lookup c inputs
handleInputs _ world = world