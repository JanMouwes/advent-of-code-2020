module Day_3.TreeTrajectory where

part1 :: (Int, Int) -> String -> Int
part1 slope = length . filter (=='#') . countTrees slope . parseGrid

part2 :: String -> Int
part2 grid = product $ map (`part1` grid) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

parseGrid :: String -> [String]
parseGrid = lines

getNextCoord :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int) 
getNextCoord (x, y) (slx, sly) width = 
    let nextY = y + sly
        nextX = (x + slx) `mod` width
    in (nextX, nextY)

countTrees :: (Int, Int) -> [String] -> String
countTrees = countTrees' (0, 0) 
    where
        countTrees' :: (Int, Int) -> (Int, Int) -> [String] -> String
        countTrees' c@(x, y) s grid 
            | y >= length grid = []
            | otherwise = 
                let (nextX, nextY) = getNextCoord (x, y) s $ length (grid !! y)
                in (grid !! y) !! x : countTrees' (nextX, nextY) s grid


