module Main where

import System.Environment ( getArgs )

solve1 :: String -> String
solve1 = undefined

solve2 :: String -> String
solve2 = undefined

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
