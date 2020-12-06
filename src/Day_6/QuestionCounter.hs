module QuestionCounter where

import Data.List

part1 :: String -> String
part1 = show . sum . map (length . nub) . findYesQs . lines

part2 :: String -> String
part2 = show . sum . map (length . findUniversal) . groupAnswers . lines

findYesQs :: [String] -> [String]
findYesQs [ln] = [ln]
findYesQs (ln:"":lns) = ln : findYesQs lns --Text followed by blank line
findYesQs (ln:nln:lns) = findYesQs ((ln ++ nln) : lns)

findUniversal :: Eq a => [[a]] -> [a]
findUniversal (x:xs) = filter (\el -> all (el `elem`) xs) x

groupAnswers :: [String] -> [[String]]
groupAnswers = foldr f []
    where 
        f :: String -> [[String]] -> [[String]]
        f s [] = [[s]]
        f "" r = []:r
        f s (p:r) = (s:p):r
