module DuDuHoX.Console where

import System.Console.ANSI
import System.IO

init :: IO ()
init = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    clearScreen
    setTitle "DuDuHoX"
    
free :: IO ()
free = do
    clearScreen
    showCursor
