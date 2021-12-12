module Main where

import Util ( mainImpl, parseNumsBy )

type StepTransformer = Int -> Int

fuelTo :: StepTransformer -> Int -> [Int] -> Int
fuelTo f p = sum . map (f . abs . (-) p)

sumTo :: Int -> Int
sumTo n = (n * (n + 1)) `div` 2

solve :: StepTransformer -> String -> Int
solve f s = let nums = parseNumsBy ',' s in minimum $ map (\n -> fuelTo f n nums) [1 .. length nums]

main :: IO ()
main = mainImpl (solve id) (solve sumTo)
