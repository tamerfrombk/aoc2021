module Main where

import Util ( mainImpl, parseNumsBy )

import Data.List ( sortBy, group )

type Point = (Int, Int)

data Line = H Point Point | V Point Point | Other Point Point
    deriving (Show, Eq)

-- I don't want to make Point a newtype because all of the pattern matching gets tedious
pointOrd :: Point -> Point -> Ordering
pointOrd (x1, y1) (x2, y2)
    | x1 == x2  = compare y1 y2
    | otherwise = compare x1 x2

trajectory :: Line -> [Point]
trajectory (H (x1, y1) (x2, _)) = [(x, y1) | x <- [x1..x2]]
trajectory (V (x1, y1) (_, y2)) = [(x1, y) | y <- [y1..y2]]
trajectory _                    = error "line is not horizontal or vertical"

parseLines :: String -> [Line]
parseLines = map parseLine . lines
    where parseLine :: String -> Line
          parseLine s = orient $ case words s of
              [p1, "->", p2] -> (parsePoint p1, parsePoint p2)
              _              -> error $ "unknown line parse: " <> s

          parsePoint :: String -> (Int, Int)
          parsePoint = (\[a, b] -> (a, b)) . parseNumsBy ','

          -- creates a vertical or horizontal line with points ordered from left to right, top to bottom
          orient :: (Point, Point) -> Line
          orient ln
            | x1 == x2  = if y1 < y2 then V (x1, y1) (x2, y2) else V (x2, y2) (x1, y1)
            | y1 == y2  = if x1 < x2 then H (x1, y1) (x2, y2) else H (x2, y2) (x1, y1)
            | otherwise = if x1 < x2 then Other (x1, y1) (x2, y2) else Other (x2, y2) (x1, y1)
            where ((x1, y1), (x2, y2)) = ln

solve1 :: String -> Int
solve1 = length . filter (\g -> length g >= 2) . group . sortBy pointOrd . concatMap trajectory . filter isHorizontalOrVertical . parseLines
    where isHorizontalOrVertical :: Line -> Bool
          isHorizontalOrVertical (H _ _) = True
          isHorizontalOrVertical (V _ _) = True
          isHorizontalOrVertical _       = False


main :: IO ()
main = mainImpl solve1 id