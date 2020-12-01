module Day_1.SumExpenses where

import Data.List

-- Part 1
find2020' :: String -> (Int, Int)
find2020' = find2020' . map read . lines

find2020 :: [Int] -> (Int, Int)
find2020 (n:ns) = case filter (==(2020-n)) ns of
    (x:_) -> (n, x)
    [] -> find2020 ns

-- Part 2
find2020s' :: String -> (Int, Int, Int)
find2020s' ns = case find2020s $ map read $ lines ns of
    (Just x) -> x
    Nothing -> (0, 0, 0)

find2020s :: [Int] -> Maybe (Int, Int, Int)
find2020s ns = case filter (\l -> length l == 3 && sum l == 2020) $ subsequences $ sort ns of
    ((x:y:z:_):_) -> Just (x, y, z)
    [] -> Nothing