module Day_16.TicketLabeler where

import qualified Data.Map as Map
import Data.List.Split
import Data.List
import Utils.List

main :: IO ()
main = do 
    input <- readFile "src/Day_16/input.txt"
    print $ part1 input
    print $ part2 input
    return ()

part1 :: String -> String
part1 = show . sum . 
    (\(rls, _, tckts) -> concatMap (`findInvalidFields` rls) tckts) . parseInput

part2 :: String -> String
part2 inp = 
    let res@(_, yr, _) = parseInput inp
        filterDepartures = filter (\(_, s) -> s `hasPrefix` "departure")
        calculate =
            product . map fst . 
            filterDepartures . 
            zip yr . map fst .
            mapValids . filterValids
    in show $ calculate res
    where 
        filterValids (rls, yr, tckts) = (rls, yr, filter (\t -> null $ findInvalidFields t rls) tckts)
        mapValids (rls, _, tckts) = findValidPermutations $ zip [0..] $ validPermutations tckts rls

validRulesFor :: Ticket -> [Rule] -> [[Rule]]
validRulesFor t rls = map (\n -> filter (fulfillsRule n) rls) t

findValidPermutations :: [(Int, [Rule])] -> [Rule]
findValidPermutations [] = []
findValidPermutations indexRs = 
    map snd 
    $ sortBy (compareBy fst) 
    $ head $ (`go` [])
    $ sortBy (compareBy (length . snd)) indexRs
    where 
        go :: [(Int, [Rule])] -> [Rule] -> [[(Int, Rule)]]
        go [] used = [[]]
        go ((index, rs):rest) used = 
            map (\r -> concatMap ((index, r):) $ go rest (r:used)) $ rs `without` used

validPermutations :: [Ticket] -> [Rule] -> [[Rule]]
validPermutations [] _ = [[]]
validPermutations ts rs = 
    let firsts = reverse $ sort $ map head ts
        validForNs ns = filter (\r -> all (`fulfillsRule` r) ns) rs
    in  map validForNs $ transpose ts

findInvalidFields :: Ticket -> [Rule] -> [Int]
findInvalidFields ns rls = filter (not . flip isFieldValid rls) ns

isFieldValid :: Int -> [Rule] -> Bool
isFieldValid n = any (fulfillsRule n)

fulfillsRule :: Int -> Rule -> Bool
fulfillsRule n (_, rls) = any (\(mn, mx) -> mn <= n && mx >= n) rls

type Range = (Int, Int)
type Rule = (String, [Range])
type Ticket = [Int]

parseInput :: String -> ([Rule], Ticket, [Ticket])
parseInput inp = 
    let (rls:yr:nrby:_) = splitOn "\n\n" inp
        rules = map parseRule $ lines rls
        your = parseTicket (lines yr !! 1)
        nearby = map parseTicket $ drop 1 $ lines nrby
    in (rules, your, nearby)

parseRule :: String -> Rule
parseRule inp = 
    let (title, rangeString) = splitOnFirst inp ':'
        (x:_:y:_) = words $ drop 1 rangeString
        parseRange :: String -> (Int, Int)
        parseRange r = (\(flr:ceil:_) -> (read flr, read ceil)) $ splitOnElement r '-'
    in (title, map parseRange [x, y])

parseTicket :: String -> Ticket
parseTicket str = map read $ splitOnElement str ','
