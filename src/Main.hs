module Main where

import Data.Maybe

import DuDuHoX.Console
import DuDuHoX.World

textWorld :: [String]
textWorld = [
    "############################",
    "# @                      ! #",
    "############################"
    ]

world :: World
world = fromJust $ parseWorld textWorld

main :: IO ()
main = do
    consoleInit
    gameLoop world
    consoleFree
    