module Main where

import           Data.Maybe

import           DuDuHoX.Console.Game
import           DuDuHoX.World
import           DuDuHoX.World.Builder

easyWorld :: [String]
easyWorld = [
    "############",
    "#.@......!.#",
    "############"
    ]

maze :: [String]
maze = [
    "#########",
    "#@#.....#",
    "#.#.###.#",
    "#....#..#",
    "#  #.##.#",
    "#....#..#",
    "#.####.##",
    "#.#.. .#",
    "#...##.#",
    "## ##..#",
    "#!....##",
    "########"
    ]

w :: World
w = fromJust . parseWorld $
    --easyWorld
    maze

main :: IO ()
main = game w
