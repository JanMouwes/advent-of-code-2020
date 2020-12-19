module Day_18.ExpressionParser (main, part1, part2) where

import Text.Parsec
import Text.Parsec.String


main :: IO ()
main = do 
    testParseResult <- readFile "src/Day_18/test_input.txt" >>= (return . parseInput)
    parseResult     <- readFile "src/Day_18/input.txt" >>= (return . parseInput)

    let (Right testInput) = testParseResult
        (Right input) = parseResult

    putStrLn $ unlines $ map show testInput
    putStrLn $ unlines $ map (show . addParens) testInput

    print "Part 1:"
    print $ part1 testInput
    print $ part1 input
    print "Part 2:"
    print $ part2 testInput
    print $ part2 input
    return ()

part1 :: Input -> String
part1 = show . sum . map evalExpr

part2 :: Input -> String
part2 = show  . sum . map weirdEval

parseInput :: String -> Either ParseError Input
parseInput = parse parser ""
    where 
        parser = try $ parseExpr `sepEndBy` endOfLine <* eof

type Input = [Expr]

data Expr = Seq SubExpr [Op]
data Op = Mult SubExpr | Sum SubExpr
data SubExpr = Parens Expr | Const Int 

instance Show Expr where
    show (Seq se ops) = show se ++ concatMap show ops

instance Show Op where
    show (Mult e) = " * " ++ show e
    show (Sum e)  = " + " ++ show e

instance Show SubExpr where
    show (Parens e) = "(" ++ show e ++ ")"
    show (Const n)  = show n

type ExprAlgebra res subRes opRes = (
        subRes -> opRes -> res,
        SubExpr -> subRes,
        [Op] -> opRes
    )

foldExpr :: ExprAlgebra r s o -> Expr -> r
foldExpr (mapExpr, mapSub, mapOps) (Seq se ops) = mapExpr (mapSub se) (mapOps ops)

evalExpr :: Expr -> Int
evalExpr = foldExpr (expr, evalSubExpr, id)
    where
        expr :: Int -> [Op] -> Int
        expr = foldl applyOp

        applyOp :: Int -> Op -> Int
        applyOp n (Mult e) = n * evalSubExpr e
        applyOp n (Sum e) = n + evalSubExpr e

        evalSubExpr :: SubExpr -> Int
        evalSubExpr (Parens e) = evalExpr e
        evalSubExpr (Const n) = n

weirdEval :: Expr -> Int
weirdEval = evalExpr . addParens

addParens :: Expr -> Expr
addParens (Seq se ops) 
    | null $ takeWhile isSum ops = Seq (addSubExprParens se) (addParens' $ map addOpParens ops)
    | otherwise = 
    let (sums, rest) = span isSum $ map addOpParens ops
        parensedSums = Parens $ Seq (addSubExprParens se) sums
    in  Seq parensedSums $ addParens' rest

isSum :: Op -> Bool
isSum (Sum _) = True
isSum _ = False

addParens' :: [Op] -> [Op]
addParens' [] = []
addParens' (Sum x:ops) = Sum x : addParens' ops
addParens' (Mult x:ops) = case span isSum ops of
    ([], _) -> Mult x:addParens' ops
    (sums, rest) -> Mult (Parens $ Seq x sums):addParens' rest

addOpParens :: Op -> Op
addOpParens (Sum x) = Sum $ addSubExprParens x
addOpParens (Mult x) = Mult $ addSubExprParens x

addSubExprParens :: SubExpr -> SubExpr 
addSubExprParens (Parens e) = Parens $ addParens e
addSubExprParens se = se

number :: Parser Int
number = read <$> many1 digit

parseExpr :: Parser Expr
parseExpr = Seq <$> pSubExpr <*> many (choice [pSum, pMult])

pOp :: String -> (SubExpr -> Op) -> Parser Op
pOp op construct = try $ construct <$> (between spaces spaces (string op) *> pSubExpr)

pSum :: Parser Op
pSum = pOp "+" Sum
    
pMult :: Parser Op
pMult = pOp "*" Mult

pConst :: Parser SubExpr
pConst = Const <$> number

pParens :: Parser SubExpr
pParens = Parens <$> between (char '(') (char ')') parseExpr

pSubExpr :: Parser SubExpr 
pSubExpr = pParens <|> pConst