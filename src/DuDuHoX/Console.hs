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
    draw player
    drawVision player world
    interactWithUser world
    where player = worldPlayer world

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
handleUserInput world (Movement m) =
    gameLoop world'
    where
        (world', _) = runUpdate world (PlayerMove m)

clearPosition :: WorldPosition -> IO ()
clearPosition = putCharAt ' '

draw :: (ShowConsole a, WorldObject a) => a -> IO ()
draw a = putCharAt (showConsole a) (worldPosition a)

drawVision :: (WorldObject a) => a -> World -> IO ()
drawVision viewer world = do
    mapM_ (\wall -> when (inSight wall) $ draw wall) walls
    when (inSight exit) $ draw exit 
    where
        walls = worldWalls world
        exit = worldExit world
        viewerPosition = worldPosition viewer
        inSight object = not $ hasAnyWallBetween world viewerPosition (worldPosition object)

putCharAt :: Char -> WorldPosition -> IO ()
putCharAt char position = setCursorPosition (y position) (x position) >> putChar char

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

class ShowConsole a where
    showConsole :: a -> Char

instance ShowConsole WorldPlayer where
    showConsole _ = '@'
    
instance ShowConsole WorldWall where
    showConsole _ = '#'
    
instance ShowConsole WorldExit where
    showConsole _ = '!'
