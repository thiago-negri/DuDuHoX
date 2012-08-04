module DuDuHoX.Console.Game where

import DuDuHoX.World
import DuDuHoX.Game
import DuDuHoX.Console.World
import DuDuHoX.Console.Core

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
        else do drawWorld w
                input <- getInput
                handleUserInput input w

win :: IO ()
win = do
    message "Voce venceu! Iupi!"
    pause

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

handleUserInput :: Maybe GameInput -> ConsoleWorld -> IO ()
handleUserInput Nothing cw = gameLoop cw
handleUserInput (Just Quit) _ = return ()
handleUserInput (Just (Movement m)) cw =
    let (w', _) = runUpdate (world cw) (PlayerMove m)
        cw' = updateWorld cw w' in
        gameLoop cw'
