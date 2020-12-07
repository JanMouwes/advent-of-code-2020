module Day_7.LuggageProcessor where

import Data.List.Split
import Data.Char
import qualified Data.Map as Map
import Data.Maybe

type Rule = (String, Int)

main :: IO ()
main = do 
    input <- readFile "src/Day_7/input.txt"
    putStrLn $ part1 input
    putStrLn $ part2 input
    return ()

part1 :: String -> String
part1 inp = 
    let ruleMap = Map.fromList $ map (createRule . concatRules . splitOnFirst (=="contain") . words) $ lines inp
    in show $ length $ filter (canHold ruleMap "shiny gold") (Map.keys ruleMap)

part2 :: String -> String
part2 inp = 
    let ruleMap = Map.fromList $ map (createRule . concatRules . splitOnFirst (=="contain") . words) $ lines inp
    in show $ ruleMap `countRequiredBags` "shiny gold"
    

canHold :: Map.Map String [Rule] -> String -> String -> Bool
canHold ruleMap bag target = Just True == do
    rules <- target `Map.lookup` ruleMap
    let (valid, invalid) = (filter ((==bag) . fst) rules, filter ((/=bag) . fst) rules)
    return $ not $ null $ valid ++ filter (canHold ruleMap bag . fst) invalid

countRequiredBags :: Map.Map String [Rule] -> String -> Int
countRequiredBags ruleMap bag = fromMaybe 1 $ do
    rules <- bag `Map.lookup` ruleMap
    let total = sum $ map (\(x, y) -> y + countRequiredBags ruleMap x * y) rules
    return total

concatRules :: ([String], [String]) -> (String, String)
concatRules (xs, ys) = (unwords $ init xs, unwords ys)
createRule :: (String, String) -> (String, [Rule])
createRule (key, vals) = 
    let rawRules = takeWhile (/='.') vals
        rulesFor x = map parseRule $ splitOn "," x
    in case rawRules of 
        "no other bags" -> (key, [])
        x -> (key, rulesFor rawRules)
parseRule :: String -> Rule
parseRule str = let (ys:xs) = words str 
                in (unwords $ init xs, read ys)


splitOnFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitOnFirst p xs = (takeWhile (not . p) xs, drop 1 $ dropWhile (not . p) xs)
