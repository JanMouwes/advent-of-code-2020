module Day_1.SumExpenses where

import Data.List

part1 :: String -> String
part1 = show . findTarget 2020 2 . map read . lines

part2 :: String -> String
part2 = show . findTarget 2020 3 . map read . lines

findTarget :: Int -> Int -> [Int] -> Maybe [Int]
findTarget t len inp = case sums t len $ sort inp of
    (x:_) -> Just x
    _ -> Nothing
    where 
        sums :: Int -> Int -> [Int] -> [[Int]]
        sums t 1 ns = map (:[]) $ filter (==t) ns
        sums t len inp = 
            let os n ns = sums (t-n) (len-1) (filter (/= n) ns)
            in concatMap (\n -> map (n:) (os n inp)) inp