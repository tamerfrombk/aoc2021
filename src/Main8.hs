module Main where

import Util ( mainImpl, count, counts )

import Data.List ( findIndex, delete, sort, sortBy, groupBy, isSubsequenceOf )
import Data.Maybe ( fromJust )

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
solve1 = count (==1) . map decodeLength . concatMap (outputs . parseSignal) . lines
    where decodeLength :: String -> Int
          decodeLength s
            | n `elem` [2,3,4,7] = 1
            | n `elem` [5, 6]    = 3
            | otherwise          = 0
            where n = length s 

groupByLengths :: [String] -> [[String]]
groupByLengths = groupBy (\a b -> length a == length b) 
                . sortBy (\a b -> compare (length a) (length b))

extractIf :: (String -> Bool) -> [String] -> (String, [String])
extractIf p ss = case filter p ss of
    (x:_) -> (x, delete x ss)
    _     -> error "extractIf: predicate failed"

decode :: [[String]] -> [String]
decode [[d1], [d7], [d4], d5s, d6s, [d8]] = let
    (d3, d5s') = extractIf (d1 `overlaps`) d5s
    (d6, d6s') = extractIf (not . overlaps d1) d6s
    (d9, [d0]) = extractIf (d4 `overlaps`) d6s'
    (d5, [d2]) = extractIf (`overlaps` d9) d5s'
    in [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9]
    where overlaps a b = isSubsequenceOf (sort a) (sort b)
decode _ = error "decode expects grouped input"

isAnagram :: String -> String -> Bool
isAnagram a b
    | length a == length b = and $ zipWith (\(x, i) (y, j) -> x == y && i == j) (counts a) (counts b)
    | otherwise            = False

fromInts :: [Int] -> Int
fromInts = foldr (\(p, n) acc -> ((10 ^ p) * n) + acc) 0 . zip [0..] . reverse

decodeSignal :: Signal -> Int
decodeSignal (Signal is os) = let 
    table = decode $ groupByLengths is
    in fromInts $ map (`decodeOutput` table) os
    where decodeOutput :: String -> [String] -> Int
          decodeOutput s = fromJust . findIndex (isAnagram s) 

solve2 :: String -> Int
solve2 = sum . map (decodeSignal . parseSignal) . lines

main :: IO ()
main = mainImpl solve1 solve2
