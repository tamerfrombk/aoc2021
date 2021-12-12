module Main where

import Util ( mainImpl, parseNumsBy )

fuelTo :: Int -> [Int] -> Int
fuelTo p = sum . map (\n ->abs $ n - p)

solve1 :: String -> Int
solve1 s = let nums = parseNumsBy ',' s in minimum $ map (`fuelTo` nums) [1 .. length nums]

solve2 :: String -> Int
solve2 = undefined

main :: IO ()
main = mainImpl solve1 solve2
