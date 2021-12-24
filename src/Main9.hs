module Main where

import Util ( mainImpl, (!?) )

import Data.Maybe ( mapMaybe, fromJust )
import Data.Char ( digitToInt )

import qualified Data.Map as M

type Cell      = (Int, Int, Int)
type HeightMap = M.Map Int [Int]

at :: Int -> Int -> HeightMap -> Maybe Int
at r c hm = hm M.!? r >>= (!? c)

adjacents :: Int -> Int -> HeightMap -> [Cell]
adjacents r c hm = mapMaybe (\(r', c') -> at r' c' hm >>= (\v -> Just (r', c', v))) [
    (r - 1, c)   -- top
    , (r, c + 1) -- right
    , (r + 1, c) -- bottom
    , (r, c - 1) -- left
    , (r, c)     -- current
    ]

trd :: (a, b, c) -> c
trd (_, _, x) = x

coord :: (a, b, c) -> (a, b)
coord (x, y, _) = (x, y)

lowPoints :: HeightMap -> [Cell]
lowPoints hm = foldr appendIfMinimum [] indexes
    where appendIfMinimum :: (Int, Int) -> [Cell] -> [Cell]
          appendIfMinimum (r, c) acc = let curr = fromJust $ at r c hm
              in case curr of
                  9 -> acc -- smoke can't settle on the highest point (9) so we exclude it
                  _ -> if curr == minimum (map trd $ adjacents r c hm) then (r, c, curr) : acc else acc
          indexes  = [(r, c) | r <- [0 .. rowCount], c <- [0 .. colCount]]
          rowCount = M.size hm - 1
          colCount = length (hm M.! 0) - 1

parseHeightMap :: String -> HeightMap
parseHeightMap = M.fromList . zip [0..] . map (map digitToInt) . lines

solve1 :: String -> Int
solve1 = sum . map (\p -> trd p + 1) . lowPoints . parseHeightMap

solve2 :: String -> String
solve2 = const "unimplemented because I can't fucking figure out how to implement DFS with visitation in Haskell"

main :: IO ()
main = mainImpl solve1 solve2
