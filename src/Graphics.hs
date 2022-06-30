module Graphics where

import Snake.Snake ( GridPoint )
import Snake.World
import Graphics.Gloss.Interface.Pure.Game
import Text.Printf ( printf )

screen :: Display
screen = InWindow "Snake" screenBounds (0, 0)

worldToPicture :: World -> Picture
worldToPicture world =
    if gameOver world
        then gameOverPicture world
        else runningGamePicture world

gameOverPicture :: World -> Picture
gameOverPicture world = Translate (fromIntegral (-(fst screenBounds)) * 0.5 + 100) 0 (Scale 0.25 0.25 (Text (printf "Score: %.2f" (score world))))

runningGamePicture :: World -> Picture
runningGamePicture world = Pictures (snakePicture ++ [foodPicture])
  where
    snakePicture = map snakePointToPicture (snake world)
    foodPicture = foodToPicture (food world)

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