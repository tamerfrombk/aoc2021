module Main where

import Util ( mainImpl )

import Data.Char ( digitToInt )

type Bit        = Int
type Binary     = [Bit]
type Diagnostic = [Binary]

makeDiagnostic :: String -> Diagnostic
makeDiagnostic = map (map digitToInt) . lines

fromBinary :: Binary -> Int
fromBinary xs = sum $ map powerOf2 $ zip [0 .. length xs - 1] (reverse xs)
    where powerOf2 = \(e, bit) -> if bit == 1 then 2 ^ e else 0

flipBit :: Bit -> Bit
flipBit 1 = 0
flipBit 0 = 1
flipBit b = error $ "invalid bit: " <> show b

mostCommonBitAt :: Int -> Diagnostic -> Bit
mostCommonBitAt p d = fromEnum $ num1s >= compliment
    where num1s       = length $ filter (==1) $ map (!! p) d
          compliment  = length d - num1s

leastCommonBitAt :: Int -> Diagnostic -> Bit
leastCommonBitAt p = flipBit . mostCommonBitAt p

gammaRate :: Diagnostic -> Binary
gammaRate xs = map ($ xs) $ mostCommonBitAt <$> [0..length (head xs) - 1]

solve1 :: String -> Int
solve1 s = fromBinary gamma * fromBinary epsilon
    where gamma      = gammaRate diagnostic
          epsilon    = map flipBit gamma
          diagnostic = makeDiagnostic s

type BitCountingStrategy = (Int -> Diagnostic -> Bit)

rating :: BitCountingStrategy -> Diagnostic -> Binary
rating strat = go 0
    where go pos ds
            | length d' == 1 = head d'
            | otherwise      = go (pos + 1) d'
            where mcb = strat pos ds
                  d'  = filter (\bs -> bs !! pos == mcb) ds

solve2 :: String -> Int
solve2 s = fromBinary o2 * fromBinary co2
    where o2         = rating mostCommonBitAt diagnostic
          co2        = rating leastCommonBitAt diagnostic
          diagnostic = makeDiagnostic s

main :: IO ()
main = mainImpl solve1 solve2
