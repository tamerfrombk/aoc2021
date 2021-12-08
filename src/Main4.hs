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

score :: Int -> Board -> Int
score n b = n * foldr (\(Unmark x) acc -> x + acc) 0 unmarkedNums
    where unmarkedNums = concatMap (filter isUnmarked) b

data Game = Game {
    draws :: [Int]
    , boards :: [Board]
} deriving Show

updateBoard :: Int -> Board -> Board
updateBoard n = map updateRow
    where updateRow :: [N] -> [N]
          updateRow = map (\n' -> if isN n n' then mark n' else n')

play :: Game -> (Board, Int)
play = loop 0
    where loop :: Int -> Game -> (Board, Int)
          loop i (Game ds bs)
            | not (null winners) = (head winners, ds !! (i - 1))
            | length ds == 1     = case filter isWinner $ update draw bs of
                                    []    -> error "a winner should always be present"
                                    (x:_) -> (x, draw)  -- we want which board will win first
            | otherwise          = loop (i + 1) $ Game { draws = ds, boards = update draw bs }
            where update :: Int -> [Board] -> [Board]
                  update n = map (updateBoard n)
                  draw     = ds !! i
                  winners  = filter isWinner bs

parseGame :: String -> Game
parseGame s = let (l:ls) = lines s in Game { draws = parseDraws l, boards = parseBoards ls }
    where parseDraws :: String -> [Int]
          parseDraws = parseNumsBy ','

          parseBoards :: [String] -> [Board]
          parseBoards ("":r1:r2:r3:r4:r5:rest) = map (map Unmark . parseNums) [r1, r2, r3, r4, r5] : parseBoards rest
          parseBoards []                       = []
          parseBoards _                        = error "invalid parse assumptions"

solve1 :: String -> Int
solve1 = (\(b, d) -> score d b) . play . parseGame

solve2 :: String -> Int
solve2 = undefined

main :: IO ()
main = mainImpl solve1 solve2
