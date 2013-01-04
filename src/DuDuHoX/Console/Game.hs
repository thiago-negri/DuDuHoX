module DuDuHoX.Console.Game where

import           DuDuHoX.Console.Interface
import           DuDuHoX.Console.Types
import           DuDuHoX.Game
import           DuDuHoX.World
import           DuDuHoX.World.Visible

game :: World -> DuDuHoXConsoleM ()
game w = do
    drawConsoleInterface
    gameLoop (mkVisWorld w)

gameLoop :: VisibleWorld -> DuDuHoXConsoleM ()
gameLoop w = do
    drawWorldInterface w
    if won (vWorld w)
        then win
        else gameLoop' w

gameLoop' :: VisibleWorld -> DuDuHoXConsoleM ()
gameLoop' w = do
    i <- getUserInput'
    handleUserInput w i

win :: DuDuHoXConsoleM ()
win = do
    message "Voce venceu! Iupi!"
    pause'

handleUserInput :: VisibleWorld -> GameInput -> DuDuHoXConsoleM ()
handleUserInput _ Quit = end'
handleUserInput consoleWorld (Movement direction) = gameLoop newConsoleWorld
    where
        newConsoleWorld = updateWorld consoleWorld newWorld
        newWorld = movePlayer (vWorld consoleWorld) direction
