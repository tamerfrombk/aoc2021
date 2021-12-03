module Main where

import Util ( mainImpl )

import Data.Char ( digitToInt )

type Diagnostic = [[Int]]

makeDiagnostic :: String -> Diagnostic
makeDiagnostic = map (map digitToInt) . lines

fromBinary :: [Int] -> Int
fromBinary xs = sum $ map powerOf2 $ zip [0 .. length xs - 1] (reverse xs)
    where powerOf2 = \(e, bit) -> if bit == 1 then 2 ^ e else 0

gammaRate :: Diagnostic -> [Int]
gammaRate xs = map (\f -> f xs) $ go <$> [0..length (head xs) - 1]
    where go p ns = if bits > half then 1 else 0
            where bits = sum $ map (at p) ns
                  half = length xs `div` 2
                  at   = flip (!!)

epsilonRate :: Diagnostic -> [Int]
epsilonRate = map flipBit . gammaRate
    where flipBit 1 = 0
          flipBit 0 = 1
          flipBit b = error $ show b <> " is not a valid bit"

solve1 :: String -> Int
solve1 s = gamma * epsilon
    where gamma      = fromBinary $ gammaRate diagnostic
          epsilon    = fromBinary $ epsilonRate diagnostic
          diagnostic = makeDiagnostic s

solve2 :: String -> Int
solve2 = undefined

main :: IO ()
main = mainImpl solve1 solve2
