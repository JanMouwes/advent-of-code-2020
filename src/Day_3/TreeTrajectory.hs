module Day_3.TreeTrajectory where

part1 :: (Int, Int) -> String -> Int
part1 slope = length . filter (=='#') . getObjectsForSlope slope . parseGrid

part2 :: String -> Int
part2 grid = product $ map (`part1` grid) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

type Grid a = [[a]]

parseGrid :: String -> [String]
parseGrid = lines

getNextCoord :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int) 
getNextCoord (x, y) (slx, sly) width = 
    let nextY = y + sly
        nextX = (x + slx) `mod` width
    in (nextX, nextY)

-- |Gets all objects on the grid in the path following a specified slope
getObjectsForSlope :: (Int, Int) -> Grid a -> [a]
getObjectsForSlope slope grid = getObjectsForSlope' (0, 0) slope (gridWidth grid) grid
    where
        getObjectsForSlope' :: (Int, Int) -> (Int, Int) -> Int -> Grid a -> [a]
        getObjectsForSlope' c@(_, y) s w grid 
            | y >= length grid = []
            | otherwise = grid `findElement` c : getObjectsForSlope' (getNextCoord c s w) s w grid
        gridWidth :: Grid a -> Int
        gridWidth g = length $ g !! 0
        findElement :: Grid a -> (Int, Int) -> a
        findElement grid (x, y) = (grid !! y) !! x

