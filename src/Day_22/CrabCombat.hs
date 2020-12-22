module Day_22.CrabCombat where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), (|>))
import Data.Foldable 
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do 
    testParseResult <- readFile "src/Day_22/test_input.txt" >>= (return . parseInput)
    parseResult     <- readFile "src/Day_22/input.txt" >>= (return . parseInput)

    let (Right testInput) = testParseResult
        (Right input) = parseResult

    print "Part 1:"
    print $ part1 testInput
    print $ part1 input
    print "Part 2:"
    print $ part2 testInput
    print $ part2 input
    return ()

part1 :: Input -> Maybe String
part1 inp = do
    player1 <- "Player 1" `Map.lookup` inp
    player2 <- "Player 2" `Map.lookup` inp
    return $ show $ calcResult 
        $ zip [1..] $ reverse $ toList 
        $ playGame player1 player2

calcResult :: [(Int, Int)] -> Int
calcResult = sum . map (uncurry (*))

playGame :: Seq.Seq Int -> Seq.Seq Int -> Seq.Seq Int
playGame Seq.Empty p2 = p2
playGame p1 Seq.Empty = p1
playGame (p1head :<| p1rest) (p2head :<| p2rest)
    | p1head > p2head = playGame (p1rest |> p1head |> p2head) p2rest
    | otherwise =       playGame p1rest (p2rest |> p2head |> p1head)

part2 :: Input -> Maybe String
part2 inp = do
    player1 <- "Player 1" `Map.lookup` inp
    player2 <- "Player 2" `Map.lookup` inp
    return $ show $ calcResult 
        $ zip [1..] $ reverse $ toList $ snd
        $ playRecursiveCombat player1 player2 Set.empty

data Player = Player1 | Player2

playRecursiveCombat :: Seq.Seq Int -> Seq.Seq Int -> Set.Set (Seq.Seq Int, Seq.Seq Int) -> (Player, Seq.Seq Int)
playRecursiveCombat Seq.Empty p2 _ = (Player2, p2)
playRecursiveCombat p1 Seq.Empty _ = (Player1, p1)
playRecursiveCombat p1 p2 prevRounds
    | (p1, p2) `Set.member` prevRounds = (Player1, p1)
playRecursiveCombat p1@(p1head :<| p1rest) p2@(p2head :<| p2rest) prevRounds
    | p1head <= length p1rest && 
      p2head <= length p2rest = 
          let newGame = playRecursiveCombat (Seq.take p1head p1rest) (Seq.take p2head p2rest) prevRounds
          in case newGame of
          (Player1, p1) -> playRecursiveCombat (p1rest |> p1head |> p2head) p2rest (Set.insert (p1, p2) prevRounds)
          (Player2, p2) -> playRecursiveCombat p1rest (p2rest |> p2head |> p1head) (Set.insert (p1, p2) prevRounds)
    | p1head > p2head = playRecursiveCombat (p1rest |> p1head |> p2head) p2rest (Set.insert (p1, p2) prevRounds)
    | otherwise =       playRecursiveCombat p1rest (p2rest |> p2head |> p1head) (Set.insert (p1, p2) prevRounds)

type Input = Map.Map String (Seq.Seq Int)

parseInput :: String -> Either ParseError Input
parseInput = parse go ""
    where 
        go = try $ do
            players <- parsePlayer `sepEndBy` newline
            eof
            return $ Map.fromList players

parsePlayer :: Parser (String, Seq.Seq Int)
parsePlayer = do
    name <- player
    cards <- number `sepEndBy` newline
    return (name, Seq.fromList cards)

player :: Parser String 
player = (\p n -> p ++ show n) <$> string "Player " <*> number <* char ':' <* newline

number :: Parser Int
number = read <$> many1 digit
