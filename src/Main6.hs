module Main where

import Util ( mainImpl )

solve1 :: String -> String
solve1 = id

solve2 :: String -> String
solve2 = id

main :: IO ()
main = mainImpl solve1 solve2