module Day_2.PasswordValidator where

part1 :: String -> Int
part1 = length . filter isPasswordValid . lines 

part2 :: String -> Int
part2 = length . filter isPasswordValid2 . lines 

type Rule = (Int, Int, Char)


splitOnFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitOnFirst p xs = (takeWhile (not . p) xs, drop 1 $ dropWhile (not . p) xs)

parseRule :: String -> Rule
parseRule rule = 
    let (bounds, ls) = splitOnFirst (==' ') rule
        (n1Str, n2Str) = splitOnFirst (=='-') bounds
        n1 = read n1Str :: Int
        n2 = read n2Str :: Int
        (letter:_) = ls
    in (n1, n2, letter)

isPasswordValid :: String -> Bool
isPasswordValid s = 
    let (rule, pass) = splitOnFirst (==':') s
        (minCount, maxCount, letter) = parseRule rule
        
        letters = filter (==letter) pass
    in length letters >= minCount && length letters <= maxCount

isPasswordValid2 :: String -> Bool
isPasswordValid2 s = 
    let (rule, pass) = splitOnFirst (==':') s
        (i, j, letter) = parseRule rule
        
        letters = filter (==letter) (pass !! i : [pass !! j])
    in length letters == 1
