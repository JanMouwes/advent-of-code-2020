module Day_19.MonsterMessages where

import qualified Data.IntMap as IMap
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do 
    testParseResult <- readFile "src/Day_19/test_input.txt" >>= (return . parseInput)
    parseResult     <- readFile "src/Day_19/input.txt" >>= (return . parseInput)

    let (Right testInput) = testParseResult
        (Right input) = parseResult

    print "Part 1:"
    print $ part1 testInput
    print $ part1 input
    print "Part 2:"
    print $ part2 testInput
    print $ part2 input
    return ()

part1 :: Input -> String
part1 (rules, strs) = show $ length $ filter (satisfiesRule rules 0) strs

satisfiesRule :: IMap.IntMap Rule -> Int -> String -> Bool
satisfiesRule rules r str = case r `IMap.lookup` rules of
    (Just rule) -> verifyRules rules str [rule]
    _ -> error "Illegal state"

verifyRules :: IMap.IntMap Rule -> String -> [Rule] -> Bool
verifyRules rules (c:str) ((Term c'):rest) = c == c' && verifyRules rules str rest
verifyRules rules str ((NonTerm opts):rest) = any (\xs -> verifyRules rules str $ addToStack rules xs rest) opts
verifyRules _ [] [] = True
verifyRules _ _ [] = False
verifyRules _ [] _ = False


addToStack :: IMap.IntMap Rule -> [Int] -> [Rule] -> [Rule]
addToStack rules ruleIds stack = mapMaybe (`IMap.lookup` rules) ruleIds ++ stack

part2 :: Input -> String
part2 (rules, strs) = 
    let newRules = IMap.adjust (mapRule 11) 11 $ IMap.adjust (mapRule 8) 8 rules -- gross
    in  show $ length $ filter (satisfiesRule newRules 0) strs 

mapRule :: Int -> Rule -> Rule
mapRule k t@(NonTerm [r:rls]) = NonTerm [r:rls, r:k:rls]
mapRule _ t = t

type Input = (IMap.IntMap Rule, [String])

parseInput :: String -> Either ParseError Input
parseInput = parse go ""
    where 
        go = try $ do
            rules <- many parseRule
            newline
            strs <- many1 anyChar
            eof
            return (IMap.fromList rules, lines strs)

parseRule :: Parser (Int, Rule)
parseRule = parseTerminal <|> parseNonTerminal
    
parseNonTerminal :: Parser (Int, Rule)
parseNonTerminal = try $ do
    key <- number
    string ": "
    rules <- nonTerm `sepBy` string "| "
    endOfLine
    return (read key, NonTerm rules)

nonTerm :: Parser [Int]
nonTerm = map read <$> number `sepEndBy` char ' '

number :: Parser String
number = many1 digit

parseTerminal :: Parser (Int, Rule)
parseTerminal = try $ do
    key <- number
    string ": "
    term <- between (char '"') (char '"') anyChar
    endOfLine
    return (read key, Term term)


data Rule = NonTerm [[Int]] | Term Char
    deriving Show

type RuleMap = IMap.IntMap Rule