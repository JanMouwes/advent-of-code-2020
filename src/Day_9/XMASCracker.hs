module Day_9.XMASCracker where

import Data.List
import Data.Maybe

type Instruction = (String, Int)

main :: IO ()
main = do 
    input <- readFile "src/Day_9/input.txt"
    print $ part1 input
    -- putStrLn $ show $ findContiguous 127 $ map (read :: String -> Int) $ lines input
    print $ part2 input >>= (Just . sort)
    return ()

part1 :: String -> Int
part1 = (\ns -> go (take 25 ns) (drop 25 ns) 2) . map (read :: String -> Int) . lines
    where
        go :: [Int] -> [Int] -> Int -> Int
        go ns@(x:source) (t:others) len =
            case findTarget t len ns of
                (Just _) -> go (source++[t]) others len
                Nothing -> t
        

part2 :: String -> Maybe [Int]
part2 inp = 
    let n = part1 inp
    in findContiguous n $ map (read :: String -> Int) $ lines inp

-- |Finds lists of specified length that sum to a target
findTarget :: Int -> Int -> [Int] -> Maybe [Int]
findTarget t len inp = case sums t len $ sort inp of
    (x:_) -> Just x
    _ -> Nothing
    where 
        sums :: Int -> Int -> [Int] -> [[Int]]
        sums t 1 inp = map (:[]) $ filter (==t) inp
        sums t len inp = 
            let os n ns = sums (t-n) (len-1) (filter (/= n) ns)
                valids = filter (<t) inp
            in concatMap (\n -> map (n:) (os n inp)) valids

-- |Finds lists of specified length that sum to a target
findContiguous :: Int -> [Int] -> Maybe [Int]
findContiguous t inp = case mapMaybe (contiguousFrom t) $ tails inp of
    (x:_) -> Just x
    _     -> Nothing
    where 
        contiguousFrom :: Int -> [Int] -> Maybe [Int]
        contiguousFrom t (x:inp)= sums (t-x) inp >>= (Just . (x:))
        
        sums :: Int -> [Int] -> Maybe [Int]
        sums t (x:mas) 
            | t > 0 = case sums (t-x) mas of
                (Just ns) -> Just (x:ns)
                Nothing -> Nothing
            | t == 0 = Just []
            | otherwise = Nothing
        sums t [] = Nothing
            
