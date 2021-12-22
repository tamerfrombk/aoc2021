module Util (
    mainImpl
    , parseNums
    , parseNumsBy
    , counts
    , count
    , (!?)
    , (<->)
    , trd
) where

import System.Environment ( getArgs )

import Data.List ( sort, group )

type Solution a = (String -> a)

mainImpl :: (Show a,  Show b) => Solution a -> Solution b -> IO ()
mainImpl f g = getArgs >>= \args -> mainImpl' args f g

mainImpl' :: (Show a,  Show b) => [String] -> Solution a -> Solution b -> IO ()
mainImpl' (n:path:_) f g
    | n == "1"  = readFile path >>= print . f
    | n == "2"  = readFile path >>= print . g
    | otherwise = error $ "invalid puzzle number" <> n
mainImpl' _ _ _ = error "invalid arguments: <puzzle> <input_file> required"

parseNums :: String -> [Int]
parseNums = map (\w -> read w :: Int) . words

parseNumsBy :: Char -> String -> [Int]
parseNumsBy c = map (\w -> read w :: Int) . wordsWhen (==c)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

counts :: Ord a => [a] -> [(a, Int)]
counts = map (\g -> (head g, length g)) . group . sort

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

(!?) :: [a] -> Int -> Maybe a
elems !? i
    | i < 0 || i >= length elems = Nothing
    | otherwise                  = Just (elems !! i)

infixr 0 <->
(<->) :: [a -> b] -> a -> [b]
fs <-> e = map ($ e) fs

trd :: (a, b, c) -> c
trd (_, _, v) = v