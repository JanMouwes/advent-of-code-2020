module Day_17.EnergyCalculator where

import qualified Data.Map as Map
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Bifunctor
import Utils.List

main :: IO ()
main = do 
    input <- readFile "src/Day_17/input.txt"
    print $ Map.filter id $ updateGrid $ parseInput input
    print $ part1 input
    print $ part2 input
    return ()

part1 :: String -> String
part1 = show . length . Map.filter id . updateNTimes 6 . parseInput 

part2 :: String -> String
part2 = show . length . Map.filter id . doNTimes updateGrid4 6 . Map.mapKeys (\(x, y, z) -> (x, y, z, 0)) . parseInput 

type Grid =  Map.Map Vec3 Bool
type Vec3 =  (Int, Int, Int)

type Grid4 =  Map.Map Vec4 Bool
type Vec4 =  (Int, Int, Int, Int)

parseInput :: String -> Grid
parseInput = Map.fromList . map (second translateChar) . concatMap (uncurry parseLine). zip [0..] . lines
    where
        parseLine :: Int -> String -> [(Vec3, Char)]
        parseLine y = zipWith (\x c -> ((x, y, 0), c)) [0..]
        translateChar :: Char -> Bool
        translateChar '.' = False
        translateChar '#' = True

updateNTimes :: Int -> Grid -> Grid
updateNTimes = doNTimes updateGrid

doNTimes :: (a -> a) -> Int -> a -> a
doNTimes f n x = foldr (\_ x -> f x) x [0..n-1]

updateGrid :: Grid -> Grid
updateGrid m = 
    let actives = filter snd $ Map.assocs m
        nbrs = filter (`Map.notMember` Map.filter id m) $ concatMap (neighbours . fst) actives
        updateBatch = nub (actives ++ map (\c -> (c, False)) nbrs)
    in Map.fromList $ map (`updateCell` m) updateBatch
    where
        updateCell :: (Vec3, Bool) -> Grid -> (Vec3, Bool)
        updateCell (c, b) g = (c, getNextStatus (c, b) g)
        getNextStatus :: (Vec3, Bool) -> Grid -> Bool
        getNextStatus (c, b) m = 
            let ns = mapMaybe (`Map.lookup` m) $ neighbours c
                actives = length $ filter id ns
            in case b of
                True  | actives == 2 -> True
                _ -> actives == 3

updateGrid4 :: Grid4 -> Grid4
updateGrid4 m = 
    let actives = filter snd $ Map.assocs m
        nbrs = filter (`Map.notMember` Map.filter id m) $ concatMap (neighbours4 . fst) actives
        updateBatch = nub (actives ++ map (\c -> (c, False)) nbrs)
    in Map.fromList $ map (`updateCell` m) updateBatch
    where
        updateCell :: (Vec4, Bool) -> Grid4 -> (Vec4, Bool)
        updateCell (c, b) g = (c, getNextStatus (c, b) g)
        getNextStatus :: (Vec4, Bool) -> Grid4 -> Bool
        getNextStatus (c, b) m = 
            let ns = mapMaybe (`Map.lookup` m) $ neighbours4 c
                actives = length $ filter id ns
            in case b of
                True  | actives == 2 -> True
                _ -> actives == 3

neighbours :: Vec3 -> [Vec3]
neighbours (x, y, z) = 
    [(x', y', z') | 
        x' <- optionsFor x,
        y' <- optionsFor y,
        z' <- optionsFor z,
        x /= x' || y /= y' || z /= z']
    where
        options = [-1, 0, 1]
        optionsFor n = map (+n) options

neighbours4 :: Vec4 -> [Vec4]
neighbours4 (x, y, z, w) = 
    [(x', y', z', w') | 
        x' <- optionsFor x,
        y' <- optionsFor y,
        z' <- optionsFor z,
        w' <- optionsFor w,
        x /= x' || y /= y' || z /= z' || w /= w']
    where
        options = [-1, 0, 1]
        optionsFor n = map (+n) options