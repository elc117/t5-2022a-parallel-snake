module Snake.MultiWorld where
import Snake.World
import Control.Parallel.Strategies

type MultiWorld = [World]

worldGridSize :: (Int, Int)
worldGridSize = (3, 3)

worldAmount :: Int
worldAmount = x * y
    where (x, y) = worldGridSize

initialMultiWorld :: MultiWorld
initialMultiWorld = parMap rseq initialWorld [1..worldAmount]