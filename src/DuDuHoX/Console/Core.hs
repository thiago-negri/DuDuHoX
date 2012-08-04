module DuDuHoX.Console.Core where

import System.Console.ANSI
import System.IO
import Control.Monad (void)

data ConsoleObject =
    ConsoleObject {
        graphic :: Char,
        translucid :: Bool,
        position :: ConsolePosition
    }
    deriving (Eq)

data ConsolePosition =
    ConsolePosition {
        consoleX :: Int,
        consoleY :: Int
    }
    deriving (Eq)

initConsole :: IO ()
initConsole = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    clearScreen
    setTitle "DuDuHoX"
    
freeConsole :: IO ()
freeConsole = do
    clearScreen
    setSGR [Reset]
    showCursor

clearConsole :: IO ()
clearConsole = clearScreen

draw :: ConsoleObject -> IO ()
draw o = graphic o `putCharAt` position o

putCharAt :: Char -> ConsolePosition -> IO ()
putCharAt c p = setCursorPosition (consoleY p) (consoleX p) >> putChar c

inSight :: IO ()
inSight = setSGR [SetColor Foreground Vivid White]

inFog :: IO ()
inFog = setSGR [SetColor Foreground Dull White]

message :: String -> IO ()
message = putStrLn

pause :: IO ()
pause = getChar >> void getChar -- pega duas vezes para garantir que o buffering do Windows não sacaneie o "pause"
