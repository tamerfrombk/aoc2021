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
mostCommonBitAt p d = fromEnum $ setBitCountAt p >= halfTotalBitCount
    where setBitCountAt pos = sum $ map (!! pos) d
          halfTotalBitCount = length d `div` 2

gammaRate :: Diagnostic -> Binary
gammaRate xs = map ($ xs) $ mostCommonBitAt <$> [0..length (head xs) - 1]

solve1 :: String -> Int
solve1 s = fromBinary gamma * fromBinary epsilon
    where gamma      = gammaRate diagnostic
          epsilon    = map flipBit gamma
          diagnostic = makeDiagnostic s

solve2 :: String -> Int
solve2 = undefined

main :: IO ()
main = mainImpl solve1 solve2
