module Main where

import Util ( mainImpl )

type Depth = Int

makeDepths :: String -> [Depth]
makeDepths = map (\c -> read c :: Depth) . lines

increases :: [Depth] -> Int
increases (x:y:xs)
    | x < y     = 1 + increases (y:xs)
    | otherwise = increases (y:xs)
increases _     = 0

solve1 :: String -> Int
solve1 = increases . makeDepths

slideSums :: [Depth] -> [Depth]
slideSums (a:b:c:rest) = sum [a,b,c] : slideSums (b:c:rest)
slideSums _            = []

solve2 :: String -> Int
solve2 = increases . slideSums . makeDepths

main :: IO ()
main = mainImpl solve1 solve2