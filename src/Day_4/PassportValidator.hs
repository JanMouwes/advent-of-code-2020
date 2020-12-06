module Day_4.PassportValidator where

import Data.Maybe
import Data.Char

expectedFields :: [String]
expectedFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

part1 :: String -> Int
part1 = length . filter hasAllExpected . map scan . splitNewlines

part2 :: String -> Int
part2 = length . filter isValid . map scan . splitNewlines

hasAllExpected :: [(String, String)] -> Bool
hasAllExpected els = 
    let keys = map fst els
    in all (`elem` keys) (filter (/="cid") expectedFields)

inRange :: Int -> (Int, Int) -> Bool
inRange n (lower, upper) = n >= lower && n <= upper

strInRange :: String -> (Int, Int) -> Bool
strInRange n rng = (read n :: Int) `inRange` rng

-- Check if valid hex colour, without regex
isColour :: String -> Bool
isColour ('#':rest) = all (\c -> isDigit c || c `elem` "abcdef") rest 
isColour _ = False

isHeight :: String -> Bool
isHeight s = go $ span isDigit s
    where 
        go (n, "cm") = n `strInRange` (150, 193)
        go (n, "in") = n `strInRange` (59, 76)
        go _ = False

isValid :: [(String, String)] -> Bool
isValid els = Just True == do
    byr <- "byr" `lookup` els
    iyr <- "iyr" `lookup` els
    eyr <- "eyr" `lookup` els
    hgt <- "hgt" `lookup` els
    hcl <- "hcl" `lookup` els
    ecl <- "ecl" `lookup` els
    pid <- "pid" `lookup` els
    let valid = byr `strInRange` (1920, 2002)
                && iyr `strInRange` (2010, 2020)
                && eyr `strInRange` (2020, 2030)
                && isHeight hgt
                && isColour hcl
                && ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                && length pid == 9 && all isDigit pid 
    return valid

parsePassports :: String -> [String]
parsePassports = go . lines 
    where 
        go :: [String] -> [String]
        go [ln] = [ln]
        go (ln:"":lns) = ln : go lns --Text followed by blank line
        go (ln:nln:lns) = go (unwords [ln, nln] : lns)

splitOnFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitOnFirst p xs = (takeWhile (not . p) xs, drop 1 $ dropWhile (not . p) xs)

scan :: String -> [(String, String)]
scan = map formTuple . words 
    where
        formTuple :: String -> (String, String)
        formTuple = splitOnFirst (==':')
