module Day_11 where

import Data.List
import Data.Maybe
import qualified Data.Tree as T
import qualified Data.Map as Map

type Instruction = (String, Int)

type Coordinate = (Int, Int)
type Grid = Map.Map Coordinate Element
data Element = Floor | Empty | Occupied deriving (Eq)

instance Show Element where
    show Floor = "."
    show Empty = "L"
    show Occupied = "#"

main :: IO ()
main = do 
    input <- readFile "src/Day_11/input.txt"
    print input
    -- print $ part1 input
    print $ part2 input
    return ()

part1 :: String -> String
part1 str = 
    let grid = parseGrid str
    in show $ countElems Occupied $ Map.elems $ updateUntil (\g -> updateGrid g == g) updateGrid grid
    where
        updateGrid :: Grid -> Grid
        updateGrid grid = Map.mapWithKey (updateCell grid) grid
            where
                updateCell :: Grid -> Coordinate -> Element -> Element
                updateCell grid c el = 
                    let ns = neighboursFor c
                        nOccupied = countElems Occupied $ mapMaybe (`Map.lookup` grid) ns
                    in case el of
                        Empty    | nOccupied == 0 -> Occupied
                        Occupied | nOccupied >= 4 -> Empty
                        oth -> oth


part2 :: String -> String
part2 str = 
    let grid = parseGrid str
    in show $ countElems Occupied $ Map.elems $ updateUntil (\g -> updateGrid g == g) updateGrid grid
    where
        updateGrid :: Grid -> Grid
        updateGrid grid = Map.mapWithKey (updateCell grid) grid
            where
                updateCell :: Grid -> Coordinate -> Element -> Element
                updateCell grid c el = 
                    let els = map snd $ visibleFrom c grid
                        nOccupied = countElems Occupied els
                    in case el of
                        Empty    | nOccupied == 0 -> Occupied
                        Occupied | nOccupied >= 5 -> Empty
                        oth -> oth

updateUntil :: (Grid -> Bool) -> (Grid -> Grid) -> Grid -> Grid
updateUntil p update grid 
    | p grid = grid
    | otherwise = updateUntil p update $ update grid

parseGrid :: String -> Grid
parseGrid = Map.fromList . map (\(coord, c) -> (coord, translateChar c)). concatMap (\(y, l) -> parseLine y l). zip [0..] . lines
    where
        parseLine :: Int -> String -> [(Coordinate, Char)]
        parseLine y = map (\(x, c) -> ((y, x), c)) . zip [0..]
        translateChar :: Char -> Element
        translateChar '.' = Floor
        translateChar 'L' = Empty
        translateChar '#' = Occupied

countElems :: Element -> [Element] -> Int
countElems el = length . filter (==el)

neighboursFor :: Coordinate -> [Coordinate]
neighboursFor (x, y) = 
    [(x+1, y), (x+1, y+1), (x, y+1), (x-1, y+1), (x-1, y), (x-1, y-1), (x, y-1), (x+1, y-1)]

add :: Coordinate -> Coordinate -> Coordinate
add (x1, y1)(x2, y2) = (x1+x2, y1+y2)

visibleFrom :: Coordinate -> Grid -> [(Coordinate, Element)]
visibleFrom c@(x, y) grid = 
    let dirs = neighboursFor (0, 0)
        findSeat c d g = 
            let newC = c `add` d
            in case newC `Map.lookup` g of
                (Just el) -> case el of
                    (Floor) -> findSeat newC d g
                    _ -> Just (newC, el)
                Nothing -> Nothing
    in mapMaybe (\d -> findSeat c d grid) dirs