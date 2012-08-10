module DuDuHoX.Console.Core where

import           Control.Monad       (void)
import           System.Console.ANSI
import           System.IO

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

cpPlus :: ConsolePosition -> ConsolePosition -> ConsolePosition
cpPlus a b = ConsolePosition (consoleX a + consoleX b) (consoleY a + consoleY b)

cpMinus :: ConsolePosition -> ConsolePosition -> ConsolePosition
cpMinus a b = ConsolePosition (consoleX b - consoleX a) (consoleY b - consoleY a)

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

drawBox :: ConsolePosition -> Int -> Int -> IO ()
drawBox p w h = do
    '+' `putCharAt` p
    '+' `putCharAt` cpPlus p (ConsolePosition (w + 1) (h + 1))
    '+' `putCharAt` cpPlus p (ConsolePosition 0 (h + 1))
    '+' `putCharAt` cpPlus p (ConsolePosition (w + 1) 0)
    drawHorizontalLine (cpPlus p (ConsolePosition 1 0)) w
    drawHorizontalLine (cpPlus p (ConsolePosition 1 (h + 1))) w
    drawVerticalLine (cpPlus p (ConsolePosition 0 1)) h
    drawVerticalLine (cpPlus p (ConsolePosition (w + 1) 1)) h

drawHorizontalLine :: ConsolePosition -> Int -> IO ()
drawHorizontalLine _ 0 = return () 
drawHorizontalLine p w = '-' `putCharAt` p >> drawHorizontalLine (cpPlus p (ConsolePosition 1 0)) (w - 1)
 
drawVerticalLine :: ConsolePosition -> Int -> IO ()
drawVerticalLine _ 0 = return () 
drawVerticalLine p h = '|' `putCharAt` p >> drawVerticalLine (cpPlus p (ConsolePosition 0 1)) (h - 1)

drawText :: ConsolePosition -> String -> IO ()
drawText _ [] = return ()
drawText p (c:cs) = c `putCharAt` p >> drawText (cpPlus p (ConsolePosition 1 0)) cs 
