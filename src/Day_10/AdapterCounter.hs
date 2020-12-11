module Day_10.AdapterCounter where

import Data.Maybe
import Data.List
import qualified Data.Map as Map

type Instruction = (String, Int)

main :: IO ()
main = do 
    input <- readFile "src/Day_10/input.txt"
    print $ part1 input
    print $ part2 input
    return ()

part1 :: String -> Int
part1 = (\(xs, _, zs) -> (length xs + 1) * (length zs + 1)) . findDiffs . sort . map (read :: String -> Int) . lines

part2 :: String -> String
part2 = show . countPaths . processInput

processInput :: String -> [Int]
processInput = sort . (\l -> l ++ [0, maximum l + 3]) . map (read :: String -> Int) . lines

countPaths :: [Int] -> Int
countPaths inp = 
    let (Just v) = 0 `Map.lookup` foldr (go $ findPossibleNexts inp) Map.empty inp 
    in v
    where
        go :: Map.Map Int [Int] -> Int -> Map.Map Int Int -> Map.Map Int Int
        go nexts curr r = case curr `Map.lookup` r of
            (Just _) -> r
            Nothing -> Map.insert curr (findVal nexts curr r) r
        findVal :: Map.Map Int [Int] -> Int -> Map.Map Int Int -> Int
        findVal nexts curr r = case curr `Map.lookup` nexts of
                (Just []) -> 1
                (Just ns) -> sum $ mapMaybe (`Map.lookup` r) ns
                Nothing -> error "value not found"

findPossibleNexts :: [Int] -> Map.Map Int [Int]
findPossibleNexts xs = Map.fromList $ map (\n -> (n, go n xs)) xs
    where
        go :: Int -> [Int] -> [Int]
        go n xs = takeWhile (<=n+3) $ dropWhile (<=n) xs


findDiffs :: [Int] -> ([(Int, Int)], [(Int, Int)], [(Int, Int)])
findDiffs = foldr f ([], [], []) . createPairs
    where
        f p@(x, y) (ones, twos, threes) = case y - x of
            1 -> (p:ones, twos, threes)
            2 -> (ones, p:twos, threes)
            3 -> (ones, twos, p:threes)
            _ -> (ones, twos, threes)

createPairs :: [Int] -> [(Int, Int)]
createPairs xs = go xs []
    where
        go :: [Int] -> [(Int, Int)] -> [(Int, Int)]
        go (x:y:zs) ps = go (y:zs) ((x, y) : ps)
        go _ ps = ps
