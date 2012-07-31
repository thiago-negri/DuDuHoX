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
    maybe (return ()) (drawUpdate world world') worldUpdate
    interactWithUser world'
    where
        (world', worldUpdate) = runUpdate world (PlayerMove m)

drawWorld :: World -> IO ()
drawWorld world = do
    mapM_ drawWall walls
    drawPlayer player 
    drawExit
    where
        walls = worldWalls world
        player = worldPlayer world
        exit = worldExit world
        drawWall wall = wall `drawAt` wallPosition wall 
        drawExit = exit `drawAt` exitPosition exit

drawUpdate :: World -> World -> WorldUpdate -> IO ()
drawUpdate oldWorld newWorld (PlayerMove _) =
    when (oldPosition /= newPosition) $ do
        clearPosition oldPosition
        drawPlayer newPlayer
    where
         newPosition = playerPosition newPlayer 
         newPlayer = worldPlayer newWorld
         oldPosition = playerPosition oldPlayer
         oldPlayer = worldPlayer oldWorld

clearPosition :: WorldPosition -> IO ()
clearPosition position  = ' ' `charAt` position

drawPlayer :: WorldPlayer -> IO ()
drawPlayer player = player `drawAt` playerPosition player

drawAt :: (ShowConsole a) => a -> WorldPosition -> IO ()
a `drawAt` position = showConsole a `charAt` position

charAt :: Char -> WorldPosition -> IO ()
char `charAt` position = setCursorPosition (y position) (x position) >> putChar char

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