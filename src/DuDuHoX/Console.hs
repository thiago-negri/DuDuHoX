module DuDuHoX.Console where

import System.Console.ANSI
import System.IO

import DuDuHoX.World
import DuDuHoX.Game

consoleInit :: IO ()
consoleInit = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    clearScreen
    setTitle "DuDuHoX"
    
consoleFree :: IO ()
consoleFree = do
    clearScreen
    showCursor

gameLoop :: World -> IO ()
gameLoop world = do
    drawWorld world
    maybeInput <- getInput
    case maybeInput of
        Nothing -> gameLoop world
        Just input -> maybe (return ()) gameLoop $ handleInput world input

drawWorld :: World -> IO ()
drawWorld world = do
    mapM_ drawWall walls
    drawPlayer 
    drawExit
    where
        walls = worldWalls world
        player = worldPlayer world
        exit = worldExit world
        drawWall wall = '#' `drawAt` wallPosition wall 
        drawPlayer = '@' `drawAt` playerPosition player
        drawExit = '!' `drawAt` exitPosition exit
        char `drawAt` position = setCursorPosition (y position) (x position) >> putChar char 

getInput :: IO (Maybe GameInput)
getInput = do
    char <- getChar
    return $! parseGameInput char

parseGameInput :: Char -> Maybe GameInput
parseGameInput 'w' = Just MoveUp
parseGameInput 's' = Just MoveDown
parseGameInput 'a' = Just MoveLeft
parseGameInput 'd' = Just MoveRight
parseGameInput 'q' = Just Quit
parseGameInput _ = Nothing
