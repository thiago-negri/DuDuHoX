module DuDuHoX.Console.Interface where

import           Control.Monad.Trans.State.Lazy
import           DuDuHoX.Console.Core
import           DuDuHoX.Console.World

drawConsoleInterface :: IO ()
drawConsoleInterface = do
    -- World area
    drawBox (ConsolePosition 1 1) 40 11

    -- Message area
    drawBox (ConsolePosition 1 15) 40 1

    -- Controls
    drawText (ConsolePosition 46 9) "(W)"
    drawText (ConsolePosition 44 10) "(A)+(D)"
    drawText (ConsolePosition 46 11) "(S)"
    drawText (ConsolePosition 46 16) "(Q)uit"

drawWorldInterface :: ConsoleWorld -> IO ()
drawWorldInterface w = do
    clearBox (ConsolePosition 2 2) 40 10
    drawWorld (execState updateWorldToInterface w)

updateWorldToInterface :: State ConsoleWorld ()
updateWorldToInterface = do
    modify $ \w -> applyDelta w (position . viewer $ w)
    modify $ flip applyDelta worldCenter
    modify $ limitWorld (ConsolePosition 2 2) (ConsolePosition 41 12)
    where
        worldCenter = ConsolePosition 21 7

message :: String -> IO ()
message = drawText (ConsolePosition 2 16)

applyDelta :: ConsoleWorld -> ConsolePosition -> ConsoleWorld
applyDelta w p =
    w {
        viewer = viewer',
        seen = seen',
        fog = fog',
        unseen = unseen'
    }
    where
        viewer' = applyDeltaPosition p (viewer w)
        seen' = map (applyDeltaPosition p) (seen w)
        fog' = map (applyDeltaPosition p) (fog w)
        unseen' = map (applyDeltaPosition p) (unseen w)

applyDeltaPosition :: ConsolePosition -> ConsoleObject -> ConsoleObject
applyDeltaPosition p o = o{position=position'}
    where position' = cpMinus (position o) p

limitWorld :: ConsolePosition -> ConsolePosition -> ConsoleWorld -> ConsoleWorld
limitWorld ur ll w =
    w {
        seen = seen',
        fog = fog',
        unseen = unseen'
    }
    where
        seen' = filter (inLimit ur ll) (seen w)
        fog' = filter (inLimit ur ll) (fog w)
        unseen' = filter (inLimit ur ll) (unseen w)

inLimit :: ConsolePosition -> ConsolePosition -> ConsoleObject -> Bool
inLimit (ConsolePosition right up) (ConsolePosition left down) o =
    let (ConsolePosition x y) = position o in
        x >= right && x <= left &&
        y >= up && y <= down
