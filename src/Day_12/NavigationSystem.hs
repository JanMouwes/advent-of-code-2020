module Day_12.NavigationSystem where

import Data.Maybe ( mapMaybe )
import Data.Bifunctor
import qualified Data.Map as Map

import Prelude hiding (Right, Left)

main :: IO ()
main = do 
    input <- readFile "src/Day_12/input.txt"
    print $ part1 input
    print $ part2 input
    return ()

part1 :: String -> String
part1 = show . manhattan (0, 0) . shipCoords . foldl (flip moveByCommand) baseShip . parseInput

part2 :: String -> String
part2 = show . manhattan (0, 0) . fst . 
    foldl (flip moveByWaypoint) ((0, 0), (10, 1)) . parseInput

baseShip :: Ship
baseShip = Ship {x=0, y=0, heading=90}

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (x2, y2) = abs x + abs x2 + abs y + abs y2

data Direction = North | South | East | West
data TurnDirection = Left | Right
data Command = DirCommand Direction Int | TurnCommand TurnDirection Int | MoveCommand Int
data Ship = Ship {x::Int, y::Int, heading::Int} deriving Show

shipCoords :: Ship -> (Int, Int)
shipCoords Ship{x=x, y=y} = (x, y)

directionVector :: Direction -> (Int, Int)
directionVector North = (0, 1)
directionVector South = (0, -1)
directionVector East = (1, 0)
directionVector West = (-1, 0)

headingDirection :: Int -> Direction
headingDirection 0 = North
headingDirection 90 = East
headingDirection 180 = South
headingDirection 270 = West
headingDirection n = error $ show n

parseInput :: String -> [Command]
parseInput = map parseCommand . lines
    where
        parseCommand :: String -> Command
        parseCommand ('N':s) = DirCommand North (read s)
        parseCommand ('S':s) = DirCommand South (read s)
        parseCommand ('E':s) = DirCommand East (read s)
        parseCommand ('W':s) = DirCommand West (read s)
        parseCommand ('L':s) = TurnCommand Left (read s)
        parseCommand ('R':s) = TurnCommand Right (read s)
        parseCommand ('F':s) = MoveCommand (read s)
        parseCommand s = error $ "unknown input " ++ s

moveByCommand :: Command -> Ship -> Ship
moveByCommand (DirCommand d i) s@Ship {x=x, y=y} = 
    let (dx, dy) = directionVector d
    in s {x=x + (dx * i), y=y + (dy * i)}
moveByCommand (TurnCommand d i) s@Ship {x=x, y=y, heading=h} = case d of
    Left -> s { heading = (360 + h-i) `mod` 360 }
    Right -> s { heading = (360 + h+i) `mod` 360}
moveByCommand (MoveCommand i) s@Ship {heading=h} = 
    let dir = headingDirection h
    in moveByCommand (DirCommand dir i) s

moveByWaypoint :: Command -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
moveByWaypoint (DirCommand d i) (c, wp) = 
    (c, moveCoordinate d i wp)
moveByWaypoint (TurnCommand d i) (c, wp) = case d of
    Left -> (c, turnWaypoint i wp )
    Right -> (c, turnWaypoint (negate i) wp )
moveByWaypoint (MoveCommand i) (c, wp) = (moveCoordinate' i wp c, wp)

turnWaypoint :: Int -> (Int, Int) -> (Int, Int)
turnWaypoint 0 (x, y) = (x, y)
turnWaypoint 90 (x, y) = turnWaypoint 0 (negate y, x) 
turnWaypoint 180 (x, y) = turnWaypoint 90 (negate y, x)
turnWaypoint 270 (x, y) = turnWaypoint 180 (negate y, x)
turnWaypoint n c = turnWaypoint ((360 + n) `mod` 360) c

moveCoordinate :: Direction -> Int -> (Int, Int) -> (Int, Int)
moveCoordinate d i c = 
    let dc = directionVector d
    in moveCoordinate' i dc c

moveCoordinate' :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveCoordinate' i (dx, dy) (x, y) = (x + (dx * i), y + (dy * i))