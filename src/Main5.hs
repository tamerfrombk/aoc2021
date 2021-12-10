module Main where

import Util ( mainImpl, parseNumsBy )

import Data.List ( sortBy, group )

type Point = (Int, Int)

data Line = H Point Point | V Point Point | D Point Point | Other
    deriving (Show, Eq)

isHV :: Line -> Bool
isHV (H _ _) = True
isHV (V _ _) = True
isHV _       = False

isHVD :: Line -> Bool
isHVD Other = False
isHVD _     = True

trajectory :: Line -> [Point]
trajectory (H (x1, y1) (x2, _))  = [(x, y1) | x <- [x1..x2]]
trajectory (V (x1, y1) (_, y2))  = [(x1, y) | y <- [y1..y2]]
trajectory (D (x1, y1) (x2, y2)) = let ys = if y1 < y2 then [y1..y2] else [y1,y1-1..y2] in zip [x1..x2] ys
trajectory _                     = error "line is not horizontal or vertical"

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
            | x1 == x2                       = if y1 < y2 then V (x1, y1) (x2, y2) else V (x2, y2) (x1, y1)
            | y1 == y2                       = if x1 < x2 then H (x1, y1) (x2, y2) else H (x2, y2) (x1, y1)
            | abs (x2 - x1) == abs (y2 - y1) = if x1 < x2 then D (x1, y1) (x2, y2) else D (x2, y2) (x1, y1)
            | otherwise                      = Other
            where ((x1, y1), (x2, y2)) = ln

solve :: (Line -> Bool) -> String -> Int
solve f = length . filter (\g -> length g >= 2) . group . sortBy pointOrd . concatMap trajectory . filter f . parseLines
    where pointOrd :: Point -> Point -> Ordering
          pointOrd (x1, y1) (x2, y2)
            | x1 == x2  = compare y1 y2
            | otherwise = compare x1 x2

main :: IO ()
main = mainImpl (solve isHV) (solve isHVD)