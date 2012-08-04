module Main where

import Data.Maybe

import DuDuHoX.Console
import DuDuHoX.World
import DuDuHoX.World.Builder

easyWorld :: [String]
easyWorld = [
    "############",
    "# @      ! #",
    "############"
    ]
    
maze :: [String]
maze = [
    "#########",
    "#@#     #",
    "# # ### #",
    "#    #  #",
    "#### ## #",
    "#    #  #",
    "# #### ##",
    "# #  # #",
    "#   ## #",
    "#####  #",
    "#!    ##",
    "########"
    ]

world :: World
world = fromJust . parseWorld $
    --easyWorld
    maze

main :: IO ()
main = do
    consoleInit
    gameLoop world
    consoleFree
    