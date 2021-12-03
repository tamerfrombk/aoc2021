module Main where

import Util ( mainImpl )
import Data.List ( foldl' )

data Command = Forward Int | Down Int | Up Int
    deriving (Show, Eq)

makeCommands :: String -> [Command]
makeCommands = map (makeCommand . words) . lines
    where makeCommand :: [String] -> Command
          makeCommand ["forward", n] = Forward (read n)
          makeCommand ["up",      n] = Up      (read n)
          makeCommand ["down",    n] = Down    (read n)
          makeCommand ws             = error $ "unrecognized command sequence: " <> unwords ws

type Position = (Int, Int)

solve1 :: String -> Int
solve1 = uncurry (*) . foldl' move (0, 0) . makeCommands
    where move :: Position -> Command -> Position
          move (h, d) (Forward n) = (h + n, d)
          move (h, d) (Up      n) = (h, d - n)
          move (h, d) (Down    n) = (h, d + n)

type AimPosition = (Int, Int, Int)

solve2 :: String -> Int
solve2 = uncurry (*) . (\(a, b, _) -> (a, b)) . foldl' moveAim (0, 0, 0) . makeCommands
    where moveAim :: AimPosition -> Command -> AimPosition
          moveAim (h, d, a) (Forward n) = (h + n, d + (a * n), a)
          moveAim (h, d, a) (Up      n) = (h, d, a - n)
          moveAim (h, d, a) (Down    n) = (h, d, a + n)

main :: IO ()
main = mainImpl solve1 solve2
