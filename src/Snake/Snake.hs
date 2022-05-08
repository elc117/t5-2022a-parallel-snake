module Snake.Snake where

data Direction = LEFT | RIGHT | UP | DOWN deriving (Eq)

type GridPoint = (Int,Int)

type Food = GridPoint
type Snake = [GridPoint]

initSnake :: Snake
initSnake = [(0,0)]

move :: Snake -> Direction -> Food -> Snake
move [] _ _ = []
move snake direction food =
    let maybeNextPoint = pointToGrow snake direction in
    case maybeNextPoint of
        Just nextPoint ->
            if nextPoint == food
                then grownSnake
                else shrink grownSnake
                    where grownSnake = grow snake nextPoint
        Nothing -> snake

grow :: Snake -> GridPoint -> Snake
grow snake nextPoint = nextPoint : snake

shrink :: Snake -> Snake
shrink [] = []
shrink snake = init snake

pointToGrow :: Snake -> Direction -> Maybe GridPoint
pointToGrow [] _ = error "Cannot find position to grow on an empty list"
pointToGrow [head] direction = Just $ addDirectionToPoint head direction
pointToGrow (head:second:body) direction
    | second == nextPoint = Nothing
    | otherwise = Just nextPoint
        where nextPoint = addDirectionToPoint head direction

addDirectionToPoint :: GridPoint -> Direction -> GridPoint
addDirectionToPoint point direction = addPoints point (toPoint direction)

addPoints :: GridPoint -> GridPoint -> GridPoint
addPoints (x, y) (u, v) = (x + u, y + v)

toPoint :: Direction -> GridPoint
toPoint direction
    | direction == LEFT = (-1, 0)
    | direction == RIGHT = (1, 0)
    | direction == UP = (0, 1)
    | direction == DOWN = (0, -1)
    | otherwise = error "Invalid Direction!"