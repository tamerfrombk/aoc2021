module Main where

import Util ( mainImpl, (!?) )

import Data.Maybe ( mapMaybe, fromJust )
import Data.Char ( digitToInt )

type HeightMap = [[Int]]

at :: Int -> Int -> HeightMap -> Maybe Int
at r c hm = hm !? r >>= (!? c)

adjacents :: Int -> Int -> HeightMap -> [Int]
adjacents r c hm = mapMaybe (\(r', c') -> at r' c' hm) [
    (r - 1, c)   -- top
    , (r, c + 1) -- right
    , (r + 1, c) -- bottom
    , (r, c - 1) -- left
    , (r, c)     -- current
    ]

findLowestPoints :: HeightMap -> [Int]
findLowestPoints hm = filter (/=9) -- smoke can't settle on the highest point
    $ foldr appendIfMinimum [] indexes
    where appendIfMinimum :: (Int, Int) -> [Int] -> [Int]
          appendIfMinimum (r, c) acc = let curr = fromJust $ at r c hm
              in if curr == minimum (adjacents r c hm) then curr : acc else acc
          indexes = [(r, c) | r <- [0 .. length hm - 1], c <- [0 .. length (head hm) - 1]]

parseHeightMap :: String -> HeightMap
parseHeightMap = map (map digitToInt) . lines

solve1 :: String -> Int
solve1 = sum . map (+1) . findLowestPoints . parseHeightMap

solve2 :: String -> Int
solve2 = undefined

main :: IO ()
main = mainImpl solve1 solve2
