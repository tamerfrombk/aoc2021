module Main where

import Util ( mainImpl )

data Command = Forward Int | Down Int | Up Int
    deriving (Show, Eq)

type Position = (Int, Int)

makeCommands :: String -> [Command]
makeCommands = map (makeCommand . words) . lines
    where makeCommand :: [String] -> Command
          makeCommand ["forward", n] = Forward (read n)
          makeCommand ["up",      n] = Up      (read n)
          makeCommand ["down",    n] = Down    (read n)
          makeCommand ws             = error $ "unrecognized command sequence: " <> unwords ws

move :: Command -> Position -> Position
move (Forward n) (h, d) = (h + n, d)
move (Up      n) (h, d) = (h, d - n)
move (Down    n) (h, d) = (h, d + n)

finalPosition :: [Command] -> Position
finalPosition = foldr move (0, 0)

solve1 :: String -> Int
solve1 = uncurry (*) . finalPosition . makeCommands

solve2 :: String -> String
solve2 = undefined

main :: IO ()
main = mainImpl solve1 solve2
