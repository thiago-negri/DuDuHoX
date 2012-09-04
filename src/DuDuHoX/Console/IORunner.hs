{-# LANGUAGE RecordWildCards #-}

module DuDuHoX.Console.IORunner
    (runConsole)
where

import           Control.Monad         (liftM, void)
import           Control.Monad.Free    (Free(..))
import           DuDuHoX.Console.Types
import           DuDuHoX.Game
import           System.Console.ANSI
import           System.IO

runConsole :: DuDuHoXConsoleM a -> IO ()
runConsole x = do
    initConsole
    runConsole' x
    freeConsole

runConsole' :: DuDuHoXConsoleM a -> IO ()
runConsole' x =
    case x of
        Free (ClearScreen{..})  -> clearScreen >> runConsole' next
        Free (DrawObjects{..})  -> (case visibility of
                                        InSight -> inSight
                                        Fog     -> inFog) >> mapM_ draw objects >> runConsole' next
        Free (ClearBox{..})     -> clearBox corner height width >> runConsole' next
        Free (DrawBox{..})      -> drawBox corner height width >> runConsole' next
        Free (DrawText{..})     -> drawText corner text >> runConsole' next
        Free (Pause{..})        -> pause >> runConsole' next
        Free (GetUserInput{..}) -> getInput >>= runConsole' . maybe x nextGI
        Free End                -> return ()
        Pure _                  -> return ()

getInput :: IO (Maybe GameInput)
getInput = liftM parseGameInput getChar

parseGameInput :: Char -> Maybe GameInput
parseGameInput 'w' = Just $ Movement MoveUp
parseGameInput 's' = Just $ Movement MoveDown
parseGameInput 'a' = Just $ Movement MoveLeft
parseGameInput 'd' = Just $ Movement MoveRight
parseGameInput 'q' = Just Quit
parseGameInput _ = Nothing

inSight :: IO ()
inSight = setSGR [SetColor Foreground Dull Black]

inFog :: IO ()
inFog = setSGR [SetColor Foreground Vivid Black]

draw :: ConsoleObject -> IO ()
draw o = graphic o `putCharAt` position o

putCharAt :: Char -> ConsolePosition -> IO ()
putCharAt c p = setCursorPosition (consoleY p) (consoleX p) >> putChar c

initConsole :: IO ()
initConsole = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setSGR [SetColor Background Vivid White,
            SetColor Foreground Dull Black]
    clearScreen
    drawBox (ConsolePosition 0 0) 52 17
    clearBox (ConsolePosition 1 1) 52 16
    setTitle "DuDuHoX"

freeConsole :: IO ()
freeConsole = do
    clearScreen
    setSGR [Reset]
    showCursor

pause :: IO ()
pause = getChar >> void getChar -- pega duas vezes para garantir que o buffering do Windows não sacaneie o "pause"

clearBox :: ConsolePosition -> Int -> Int -> IO ()
clearBox p w h
    | h < 0 = return ()
    | otherwise = drawText (cpPlus p (ConsolePosition 0 h)) (replicate w ' ') >> clearBox p w (h - 1)

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
drawHorizontalLine p w = drawText p $ replicate w '-'

drawVerticalLine :: ConsolePosition -> Int -> IO ()
drawVerticalLine _ 0 = return ()
drawVerticalLine p h = '|' `putCharAt` p >> drawVerticalLine (cpPlus p (ConsolePosition 0 1)) (h - 1)

drawText :: ConsolePosition -> String -> IO ()
drawText p t = setCursorPosition (consoleY p) (consoleX p) >> putStr t
