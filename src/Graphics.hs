module Graphics where

import Snake.Snake ( GridPoint )
import Snake.World
import Graphics.Gloss.Interface.Pure.Game
import Text.Printf ( printf )
import Snake.MultiWorld (MultiWorld, worldGridSize)
import Control.Parallel.Strategies

screenBounds :: (Int, Int)
screenBounds = (600, 600)

tileSize :: (Float, Float)
tileSize = (fromIntegral sw / fromIntegral gw / fromIntegral ww, fromIntegral sh / fromIntegral gh / fromIntegral wh)
  where
    (ww, wh) = worldGridSize
    (gw, gh) = gridSize
    (sw, sh) = screenBounds

worldSize :: (Float, Float)
worldSize = (tx * fromIntegral gx, ty * fromIntegral gy)
  where
    (gx, gy) = gridSize
    (tx, ty) = tileSize

maxWorldsPerLine :: Int
maxWorldsPerLine = round (fromIntegral sw / ww)
  where
    (ww, wh) = worldSize
    (sw, sh) = screenBounds

properWorldPosition :: Int -> (Float, Float)
properWorldPosition index = properPosition
  where
    properPosition = (worldW * fromIntegral gx + halfWorldW - halfW, worldH * fromIntegral gy + halfWorldH - halfH)
    (halfW, halfH) = (fromIntegral w / 2, fromIntegral h / 2)
    (w, h) = screenBounds
    (halfWorldW, halfWorldH) = (worldW / 2, worldH / 2)
    (worldW, worldH) = worldSize
    (gx, gy) = gridPostition
    gridPostition = (index `mod` maxWorldsPerLine, index `div` maxWorldsPerLine)

screen :: Display
screen = InWindow "Snake" screenBounds (0, 0)

translateByCoordinates :: Picture -> (Float, Float) -> Picture
translateByCoordinates picture (x, y) = Translate x y picture

worldToPicture :: World -> Picture
worldToPicture world =
    if gameOver world
        then gameOverPicture world
        else runningGamePicture world

backGroundColors :: [Color]
backGroundColors = map greyN [0.1, 0.2..1]

multiWorldToPicture :: MultiWorld -> Picture
multiWorldToPicture multiWorld = multiWorldPicture
  where
    multiWorldPicture = Pictures porperlyPositionedWorldPictures
    properlyPositionedWorldPicture index worldPicture = translateByCoordinates worldPicture (properWorldPosition index)
    porperlyPositionedWorldPictures = zipWith properlyPositionedWorldPicture [0..] worldPictures
    worldPictures = parMap rpar worldToPicture multiWorld

gameOverPicture :: World -> Picture
gameOverPicture world = Translate (-fst worldSize * 0.5 + 50) 0 (Scale 0.1 0.1 (Text (printf "Score: %.2f" (score world))))

runningGamePicture :: World -> Picture
runningGamePicture world = Pictures ([Color (backGroundColors !! (seed world `mod` length backGroundColors)) entireGridPicture, snakePicture] ++ [foodPicture])
  where
    snakePicture = Pictures (map snakePointToPicture (snake world))
    foodPicture = foodToPicture (food world)

snakePointToPicture :: GridPoint -> Picture
snakePointToPicture snakePoint = Color black (gridPointToPicture snakePoint)

foodToPicture :: GridPoint -> Picture
foodToPicture food = Color red (gridPointToPicture food)

entireGridPicture :: Picture
entireGridPicture =
  Polygon [tl, tr, br, bl]
  where
    (worldW, worldH) = worldSize
    (startx, starty) = (- worldW / 2, - worldH / 2)
    tl = (startx, starty)
    tr = (startx + worldW, starty)
    br = (startx + worldW, starty + worldH)
    bl = (startx, starty + worldH)

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