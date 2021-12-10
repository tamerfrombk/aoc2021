module Main where

import Util ( mainImpl, parseNumsBy )

newtype Fish = Fish Int
    deriving (Show, Eq)

tick :: Fish -> [Fish]
tick (Fish n)
    | n == 0    = [Fish 6, Fish 8]
    | otherwise = [Fish (n - 1)]

lifetime :: Int -> [Fish] -> [Fish]
lifetime n = go 0
    where go :: Int -> [Fish] -> [Fish]
          go i fs
            | i == n    = fs
            | otherwise = concatMap tick $ go (i + 1) fs

parseFish :: String -> [Fish]
parseFish = map Fish . parseNumsBy ','

solve1 :: String -> Int
solve1 = length . lifetime 80 . parseFish

solve2 :: String -> String
solve2 = id

main :: IO ()
main = mainImpl solve1 solve2