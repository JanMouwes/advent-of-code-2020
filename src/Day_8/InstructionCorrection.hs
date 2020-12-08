module Day_8.InstructionCorrection where

import Data.List
import Data.Maybe

type Instruction = (String, Int)

main :: IO ()
main = do 
    input <- readFile "src/Day_8/input.txt"
    putStrLn $ part1 input
    putStrLn $ part2 input
    return ()

part1 :: String -> String
part1 = show . startExecution . map parseInstruction . lines

part2 :: String -> String
part2 inp = 
    let instructions = map parseInstruction $ lines inp
        Just (_, _, stack) = startExecution instructions
    in show 
       $ maximumBy (\(_, _, x:_)(_, _, y:_) -> x `compare` y) 
       $ mapMaybe (startExecution . (`swapInstruction` instructions)) stack

swapInstruction :: Int -> [Instruction] -> [Instruction]
swapInstruction i instrs = 
    let (begin,x:end) = splitAt i instrs 
        swap :: Instruction -> Instruction
        swap ("nop", arg) = ("jmp", arg)
        swap ("jmp", arg) = ("nop", arg)
        swap (ins, arg) = (ins, arg)
    in begin ++ [swap x] ++ end

parseInstruction :: String -> Instruction
parseInstruction str = 
    let [ins, arg] = words str
    in (ins, read $ filter (/='+') arg)

startExecution :: [Instruction] -> Maybe (Int, Int, [Int])
startExecution x = executeInstructions x [] 0 0

executeInstructions :: [Instruction] -> [Int] -> Int -> Int -> Maybe (Int, Int, [Int])
executeInstructions instrs prevs index acc 
    | index > length instrs = Nothing
    | index == length instrs = Just (index, acc, prevs)
    | otherwise = 
    case executeInstruction (instrs !! index) index acc of
        (newIndex, newAcc) 
            | newIndex `elem` prevs  -> Just (newIndex, newAcc, prevs)
            | otherwise -> executeInstructions instrs (newIndex : prevs) newIndex newAcc

executeInstruction :: Instruction -> Int -> Int -> (Int, Int)
executeInstruction ("nop", _) index acc = (index + 1, acc)
executeInstruction ("acc", x) index acc = (index + 1, acc + x)
executeInstruction ("jmp", x) index acc = (index + x, acc)
executeInstruction _ index acc = (index, acc)
