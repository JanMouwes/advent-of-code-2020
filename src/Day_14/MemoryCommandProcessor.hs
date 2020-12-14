module Day_14.MemoryCommandProcessor where

import Data.List
import qualified Data.Map as Map
import Data.List.Split
import Text.Read
import Data.Char
import Numeric

import Prelude hiding (Right, Left)

main :: IO ()
main = do 
    input <- readFile "src/Day_14/input.txt"
    print $ part1 input
    print $ part2 input
    return ()

part1 :: String -> String
part1 = show . sum . Map.elems . snd . foldl (flip execute) ("", Map.empty) . parseInput
    where 
        execute :: Command -> (String, Map.Map Int Int) -> (String, Map.Map Int Int)
        execute (Mask mask) (_, mem) = (mask, mem)
        execute (Write addr val) (mask, mem) = (mask, Map.insert addr (applyMask mask val) mem)
        applyMask :: String -> Int -> Int
        applyMask mask num = 
            let bound = zip mask $ pad (length mask) '0' $ toBin num
                applyMaskElement :: Char -> Char -> Char
                applyMaskElement 'X' n = n
                applyMaskElement m n = m
            in toDec $ map (uncurry applyMaskElement) bound

part2 :: String -> String
part2 = show . sum . Map.elems . snd . 
    foldl (flip execute) ("", Map.empty) . parseInput
    where 
        execute :: Command -> (String, Map.Map Int Int) -> (String, Map.Map Int Int)
        execute (Mask mask) (_, mem) = (mask, mem)
        execute (Write addr val) (mask, mem) = 
            let addrs = applyMask mask addr
            in (mask, foldr (`Map.insert` val) mem addrs)
        applyMask :: String -> Int -> [Int]
        applyMask mask num = 
            let bound = zip mask $ pad (length mask) '0' $ toBin num
                applyMaskElement :: (Char, Char) -> [String] -> [String]
                applyMaskElement ('0', n) strs = map (n:) strs
                applyMaskElement ('1', _) strs = map ('1':) strs
                applyMaskElement ('X', _) strs = concatMap (\n -> map (n:) strs) ['0', '1']
            in map read $ foldl (flip applyMaskElement) [[]] bound

parseInput :: String -> [Command]
parseInput = map parseLine . lines
    where 
        parseLine :: String -> Command
        parseLine line = case splitOn " = " line of
            ["mask", mask] -> Mask mask
            [mem, val] ->
                let addr = takeWhile isDigit $ drop 4 mem
                in Write (read addr) (read val)

data Command = Mask String | Write Int Int

pad :: Int -> a -> [a] -> [a]
pad len el list = replicate (len - length list) el ++ list

toBin :: Int -> String
toBin n = showIntAtBase 2 intToDigit n ""

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0