module Main where

import Util ( mainImpl, count )

decode :: String -> [Int]
decode s
    | n == 2    = [1]
    | n == 3    = [7]
    | n == 4    = [4]
    | n == 5    = [3, 5]
    | n == 6    = [6, 0, 9]
    | n == 7    = [8]
    | otherwise = []
    where n     = length s

data Signal = Signal {
    inputs    :: [String]
    , outputs :: [String]
} deriving Show

parseSignal :: String -> Signal
parseSignal s = case words s of
    [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, "|", o1, o2, o3, o4]
        -> Signal [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10] [o1, o2, o3, o4]
    _   -> error "invalid signal format"

solve1 :: String -> Int
solve1 = count (\g -> length g == 1) . map decode . concatMap (outputs . parseSignal) . lines

solve2 :: String -> Int
solve2 = undefined

main :: IO ()
main = mainImpl solve1 solve2
