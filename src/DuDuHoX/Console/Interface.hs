module DuDuHoX.Console.Interface where

import DuDuHoX.Console.World
import DuDuHoX.Console.Core

data ConsoleInterface =
    ConsoleInterface {
        ciWorld :: ConsoleWorld
    }

drawConsoleInterface :: ConsoleInterface -> IO ()
drawConsoleInterface ci = do
    -- World area
    drawBox (ConsolePosition 1 1) 40 11
    drawBox (ConsolePosition 1 15) 40 1
    
    -- Controls
    drawText (ConsolePosition 46 9) "(W)"
    drawText (ConsolePosition 44 10) "(A)+(D)"
    drawText (ConsolePosition 46 11) "(S)"
    drawText (ConsolePosition 46 16) "(Q)uit"

    -- World
    drawWorld world'''
    where
        player = viewer w
        w = ciWorld ci
        world' = applyDelta w (position player)
        world'' = applyDelta world' worldCenter
        world''' = limitWorld (ConsolePosition 2 2) (ConsolePosition 41 12) world''
        worldCenter = ConsolePosition 21 7

applyDelta :: ConsoleWorld -> ConsolePosition -> ConsoleWorld
applyDelta w p = w {
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
limitWorld ur ll w = w {
        seen = seen',
        fog = fog',
        unseen = unseen'
    }
    where
        seen' = filter (inLimit ur ll) (seen w)
        fog' = filter (inLimit ur ll) (fog w)
        unseen' = filter (inLimit ur ll) (unseen w)

inLimit (ConsolePosition right up) (ConsolePosition left down) o =
    let (ConsolePosition x y) = position o in
        x >= right && x <= left &&
        y >= up && y <= down