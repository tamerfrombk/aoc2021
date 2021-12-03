module Util (
    mainImpl
) where

import System.Environment ( getArgs )

type Solution a = (String -> a)

mainImpl :: (Show a,  Show b) => Solution a -> Solution b -> IO ()
mainImpl f g = getArgs >>= \args -> mainImpl' args f g

mainImpl' :: (Show a,  Show b) => [String] -> Solution a -> Solution b -> IO ()
mainImpl' (n:path:_) f g
    | n == "1"  = readFile path >>= print . f
    | n == "2"  = readFile path >>= print . g
    | otherwise = error $ "invalid puzzle number" <> n
mainImpl' _ _ _ = error "invalid arguments: <puzzle> <input_file> required"