module Main where

import Util ( mainImpl, parseNums, parseNumsBy )

import Data.List ( transpose )

data N = Mark Int | Unmark Int
    deriving ( Show, Eq )

mark :: N -> N
mark (Unmark n) = Mark n
mark n          = n

isMarked :: N -> Bool
isMarked (Mark _) = True
isMarked _        = False

isUnmarked :: N -> Bool
isUnmarked = not . isMarked

isN :: Int -> N -> Bool
isN x (Mark y)   = x == y
isN x (Unmark y) = x == y

type Board = [[N]]

isWinner :: Board -> Bool
isWinner b = any (any (all isMarked)) [b, transpose b]

data Game = Game {
    draws :: [Int]
    , boards :: [Board]
} deriving Show

update :: Int -> [Board] -> [Board]
update n = map (map updateRow)
    where updateRow :: [N] -> [N]
          updateRow = map (\e -> if isN n e then mark e else e)

parseGame :: String -> Game
parseGame s = let (l:ls) = lines s in Game { draws = parseDraws l, boards = parseBoards ls }
    where parseDraws :: String -> [Int]
          parseDraws = parseNumsBy ','

          parseBoards :: [String] -> [Board]
          parseBoards ("":r1:r2:r3:r4:r5:rest) = map (map Unmark . parseNums) [r1, r2, r3, r4, r5] : parseBoards rest
          parseBoards []                       = []
          parseBoards _                        = error "invalid parse assumptions"

play1 :: Game -> (Board, Int)
play1 = loop 0
    where loop :: Int -> Game -> (Board, Int)
          loop i (Game ds bs)
            | not (null winners) = (head winners, prevDraw)
            | otherwise          = loop (i + 1) Game { draws = ds, boards = bs' }
            where draw     = ds !! i
                  prevDraw  = ds !! (i - 1)
                  bs'      = update draw bs
                  winners  = filter isWinner bs

play2 :: Game -> (Board, Int)
play2 = loop 0
    where loop :: Int -> Game -> (Board, Int)
          loop i (Game ds bs)
            | length bs == 1 = if isWinner b then (b, prevDraw) else loop (i + 1) (Game ds bs')
            | otherwise      = loop (i + 1) Game { draws = ds, boards = remaining }
            where draw      = ds !! i
                  prevDraw  = ds !! (i - 1)
                  bs'       = update draw bs
                  remaining = filter (not . isWinner) bs'
                  b         = head bs

type Play = Game -> (Board, Int)

solve :: Play -> String -> Int
solve p = (\(b, d) -> d * score b) . p . parseGame
    where score :: Board -> Int
          score = foldr (\(Unmark x) acc -> x + acc) 0 . concatMap (filter isUnmarked)

main :: IO ()
main = mainImpl (solve play1) (solve play2)
