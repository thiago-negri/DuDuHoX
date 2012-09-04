module DuDuHoX.Console.Game where

import           DuDuHoX.Console.Interface
import           DuDuHoX.Console.Types
import           DuDuHoX.Console.World
import           DuDuHoX.Game
import           DuDuHoX.World

game :: World -> DuDuHoXConsoleM ()
game w = do
    drawConsoleInterface
    gameLoop (mkWorld w)

gameLoop :: ConsoleWorld -> DuDuHoXConsoleM ()
gameLoop w = do
    drawWorldInterface w
    if won (world w)
        then win
        else gameLoop' w

gameLoop' :: ConsoleWorld -> DuDuHoXConsoleM ()
gameLoop' w = do
    i <- getUserInput'
    handleUserInput w i

win :: DuDuHoXConsoleM ()
win = do
    message "Voce venceu! Iupi!"
    pause'

handleUserInput :: ConsoleWorld -> GameInput -> DuDuHoXConsoleM ()
handleUserInput _ Quit = end'
handleUserInput consoleWorld (Movement direction) = gameLoop newConsoleWorld
    where
        newConsoleWorld = updateWorld consoleWorld newWorld
        newWorld = runUpdate (world consoleWorld) (PlayerMove direction)
