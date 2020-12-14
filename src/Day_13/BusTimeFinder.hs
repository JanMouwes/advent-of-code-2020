module Day_13.BusTimeFinder where

import Data.List
import Data.List.Split
import Text.Read

import Prelude hiding (Right, Left)

main :: IO ()
main = do 
    input <- readFile "src/Day_13/input.txt"
    print $ part1 input
    print $ part2 input
    return ()

part1 :: String -> String
part1 inp = 
    let (time, buses) = (\[x, y] -> (read x :: Int, map (read :: String -> Int) $ filter (/="x")$ splitOn "," y)) . lines $ inp
        (busId, depart) =  findNearestBus time buses
    in show $ busId * (depart - time)

part2 :: String -> String
part2 inp = 
    let (_, buses) = (\[x, y] -> (x, map (readMaybe :: String -> Maybe Int) $ splitOn "," y)) $ lines inp
        indexedBuses = map (\(i, Just b) -> (i, b)) $ filter ((/=Nothing) . snd) $ zip [0..] buses
    in show $ findValidTime indexedBuses

findValidTime :: [(Int, Int)] -> Int
findValidTime buses = fst $ foldl findNextSolution (0, 1) buses
    where
        findNextSolution :: (Int, Int) -> (Int, Int) -> (Int, Int)
        findNextSolution (sol, step) iBus = findTimestamp iBus (sol, step)

findTimestamp :: (Int, Int) -> (Int, Int) -> (Int, Int)
findTimestamp (offset, bus) (sol, step) = 
    let steps = map (\n -> sol + n *step) [0..]
    in head $ map (\ts -> (ts, step * bus)) $ filter (\ts -> (ts + offset) `mod` bus == 0) steps

findNearestBus :: Int -> [Int] -> (Int, Int)
findNearestBus time buses = 
    let sorted = reverse $ sort buses
    in minimumBy (\(_, x) (_, y) -> x `compare` y) $ map (\bus -> (bus, findNearestDeparture time bus)) sorted

findNearestDeparture :: Int -> Int -> Int
findNearestDeparture time bus = 
    let remainder = time `mod` bus
    in time + (bus - remainder)

