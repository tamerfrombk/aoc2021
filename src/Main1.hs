module Main where

import System.Environment ( getArgs )

type Depth = Int

makeDepths :: String -> [Depth]
makeDepths = map (\c -> read c :: Depth) . lines

increases :: [Depth] -> Int
increases (x:y:xs)
    | x < y     = 1 + increases (y:xs)
    | otherwise = increases (y:xs)
increases _     = 0

solve1 :: String -> String
solve1 = show . increases . makeDepths

slideSums :: [Depth] -> [Depth]
slideSums (a:b:c:rest) = sum [a,b,c] : slideSums (b:c:rest)
slideSums _            = []

solve2 :: String -> String
solve2 = show . increases . slideSums . makeDepths

mainWithArgs :: [String] -> IO ()
mainWithArgs (n:path:_)
    | n == "1"  = invoke solve1
    | n == "2"  = invoke solve2
    | otherwise = error $ "invalid puzzle number" <> n
    where invoke :: (String -> String) -> IO ()
          invoke f = readFile path >>= putStrLn . f
mainWithArgs _  = error "invalid arguments: <puzzle> <input_file> required"

main :: IO ()
main = getArgs >>= mainWithArgs
