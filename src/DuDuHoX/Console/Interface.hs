module DuDuHoX.Console.Interface where

import           Control.Monad.Trans.State.Lazy
import           DuDuHoX.Console.Types
import           DuDuHoX.World.Types
import           DuDuHoX.World.Visible

drawConsoleInterface :: DuDuHoXConsoleM ()
drawConsoleInterface = do
    -- World area
    drawBox' (ConsolePosition 2 1) 39 11

    -- Message area
    drawBox' (ConsolePosition 2 15) 39 1

    -- Controls
    drawText' (ConsolePosition 46 9) "(W)"
    drawText' (ConsolePosition 44 10) "(A)+(D)"
    drawText' (ConsolePosition 46 11) "(S)"
    drawText' (ConsolePosition 46 16) "(Q)uit"

drawWorldInterface :: VisibleWorld -> DuDuHoXConsoleM ()
drawWorldInterface w = do
    clearBox' (ConsolePosition 3 2) 39 10
    drawWorld (execState updateWorldToInterface w)

drawWorld :: VisibleWorld -> DuDuHoXConsoleM ()
drawWorld w = do
    drawObjects' Fog (fog w)
    drawObjects' InSight (seen w ++ [viewer w])

updateWorldToInterface :: State VisibleWorld ()
updateWorldToInterface = do
    modify $ limitViewer (WorldPosition 19 5)
    modify $ applyDeltaW (WorldPosition 3 2)

message :: String -> DuDuHoXConsoleM ()
message = drawText' (ConsolePosition 3 16)
