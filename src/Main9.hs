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

calculateRiskOfLowestPoints :: HeightMap -> [Int]
calculateRiskOfLowestPoints hm = foldr appendIfMinimum [] indexes
    where appendIfMinimum :: (Int, Int) -> [Int] -> [Int]
          appendIfMinimum (r, c) acc = let curr = fromJust $ at r c hm
              in case curr of
                  9 -> acc -- smoke can't settle on the highest point (9) so we exclude it
                  _ -> if curr == minimum (adjacents r c hm) then curr + 1 : acc else acc
          indexes = [(r, c) | r <- [0 .. length hm - 1], c <- [0 .. length (head hm) - 1]]

parseHeightMap :: String -> HeightMap
parseHeightMap = map (map digitToInt) . lines

solve1 :: String -> Int
solve1 = sum . calculateRiskOfLowestPoints . parseHeightMap

solve2 :: String -> Int
solve2 = undefined

main :: IO ()
main = mainImpl solve1 solve2
