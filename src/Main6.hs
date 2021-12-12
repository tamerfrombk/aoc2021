module Main where

import Util ( mainImpl, parseNumsBy, counts )

type Fish = Int

parseFish :: String -> [Fish]
parseFish = map (snd . (\(n, c) -> (n, c - 1))) . counts . (++[0..8]) . parseNumsBy ','

solve :: Int -> String -> Int
solve days = sum . (!! days) . iterate shift . parseFish
    where shift :: [Fish] -> [Fish]
          shift [x0, x1, x2, x3, x4, x5, x6, x7, x8] = [x1, x2, x3, x4, x5, x6, x7 + x0, x8, x0]
          shift _                                    = error "wrong number of fish"

main :: IO ()
main = mainImpl (solve 80) (solve 256)