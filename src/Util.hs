module Util (
    mainImpl
    , wordsWhen
    , parseNums
    , parseNumsBy
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

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseNums :: String -> [Int]
parseNums = map (\w -> read w :: Int) . words

parseNumsBy :: Char -> String -> [Int]
parseNumsBy c = map (\w -> read w :: Int) . wordsWhen (==c)