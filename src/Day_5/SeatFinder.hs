module Day_5.SeatFinder where

import Data.List

part1 :: String -> Int
part1 = maximum . map (toId . parseSeat) .  lines

part2 :: String -> Int
part2 = findNonConseq . sort . map (toId . parseSeat) . lines

toId :: (Int, Int) -> Int
toId (r, c) = r * 8 + c

findNonConseq :: [Int] -> Int
findNonConseq (x:y:xs) 
    | x == (y-1) = findNonConseq (y:xs)
    | otherwise = x+1
findNonConseq (x:xs) = x+1

splitOnFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitOnFirst p xs = (takeWhile (not . p) xs, dropWhile (not . p) xs)

parseSeat :: String -> (Int, Int)
parseSeat xs = 
    let (row, col) = splitOnFirst (\c -> c=='R' || c=='L') xs
    in (parseRow row, parseColumn col)

parseRow :: String -> Int
parseRow = parseBinary . map (== 'B')

parseColumn :: String -> Int
parseColumn = parseBinary . map (== 'R')

parseBinary :: [Bool] -> Int
parseBinary = foldl f 0
    where 
        f :: Int -> Bool -> Int
        f r True  = 2 * r + 1
        f r False  = 2 * r
