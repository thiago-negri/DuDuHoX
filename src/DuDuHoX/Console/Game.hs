module DuDuHoX.Console.Game where

import           Control.Monad         (liftM)
import           DuDuHoX.Console.Core
import           DuDuHoX.Console.Interface
import           DuDuHoX.Console.World
import           DuDuHoX.Game
import           DuDuHoX.World

game :: World -> IO ()
game w = do
    initConsole
    gameLoop $ mkWorld w
    freeConsole

gameLoop :: ConsoleWorld -> IO ()
gameLoop w = do
    clearConsole
    if won (world w)
        then win
        else do drawConsoleInterface (ConsoleInterface w)
                input <- getInput
                maybe (gameLoop w) (handleUserInput w) input

win :: IO ()
win = do
    message "Voce venceu! Iupi!"
    pause

getInput :: IO (Maybe GameInput)
getInput = liftM parseGameInput getChar

parseGameInput :: Char -> Maybe GameInput
parseGameInput 'w' = Just $ Movement MoveUp
parseGameInput 's' = Just $ Movement MoveDown
parseGameInput 'a' = Just $ Movement MoveLeft
parseGameInput 'd' = Just $ Movement MoveRight
parseGameInput 'q' = Just Quit
parseGameInput _ = Nothing

handleUserInput :: ConsoleWorld -> GameInput -> IO ()
handleUserInput _ Quit = return ()
handleUserInput consoleWorld (Movement direction) = gameLoop newConsoleWorld
    where
        newConsoleWorld = updateWorld consoleWorld newWorld
        newWorld = runUpdate (world consoleWorld) (PlayerMove direction)
