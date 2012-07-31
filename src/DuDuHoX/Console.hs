module DuDuHoX.Console where

import System.Console.ANSI
import System.IO

import DuDuHoX.World
import DuDuHoX.Game

import Control.Monad

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
    clearScreen
    drawWorld world
    interactWithUser world

interactWithUser :: World -> IO ()
interactWithUser world =
    if playerPosition player == exitPosition exit
        then win
        else do
            maybeInput <- getInput
            case maybeInput of
                Nothing -> interactWithUser world
                Just input -> handleUserInput world input
    where
        player = worldPlayer world
        exit = worldExit world

win :: IO ()
win = do
    clearScreen
    setCursorPosition 0 0
    putStrLn "Voce venceu!!! Iupi!!!"
    consolePause

consolePause :: IO ()
consolePause = getChar >> void getChar -- Maldito Windows (buffering do console) 

handleUserInput :: World -> GameInput -> IO ()
handleUserInput _ Quit = return ()
handleUserInput world (Movement m) = do
    case worldUpdate of
        Just update -> drawUpdate world world' update
        Nothing -> return ()
    interactWithUser world'
    where
        (world', worldUpdate) = runUpdate world (PlayerMove m)

drawWorld :: World -> IO ()
drawWorld world = do
    mapM_ drawWall walls
    drawPlayer $ playerPosition player 
    drawExit
    where
        walls = worldWalls world
        player = worldPlayer world
        exit = worldExit world
        drawWall wall = '#' `drawAt` wallPosition wall 
        drawExit = '!' `drawAt` exitPosition exit

drawUpdate :: World -> World -> WorldUpdate -> IO ()
drawUpdate oldWorld newWorld (PlayerMove _) =
    when (oldPosition /= newPosition) $ do
        clearPosition oldPosition
        drawPlayer newPosition
    where
         newPosition = playerPosition newPlayer 
         newPlayer = worldPlayer newWorld
         oldPosition = playerPosition oldPlayer
         oldPlayer = worldPlayer oldWorld

drawPlayer :: WorldPosition -> IO ()
drawPlayer position = '@' `drawAt` position

clearPosition :: WorldPosition -> IO ()
clearPosition position  = ' ' `drawAt` position

drawAt :: Char -> WorldPosition -> IO ()
char `drawAt` position = setCursorPosition (y position) (x position) >> putChar char

getInput :: IO (Maybe GameInput)
getInput = do
    char <- getChar
    return $! parseGameInput char

parseGameInput :: Char -> Maybe GameInput
parseGameInput 'w' = Just $ Movement MoveUp
parseGameInput 's' = Just $ Movement MoveDown
parseGameInput 'a' = Just $ Movement MoveLeft
parseGameInput 'd' = Just $ Movement MoveRight
parseGameInput 'q' = Just Quit
parseGameInput _ = Nothing
