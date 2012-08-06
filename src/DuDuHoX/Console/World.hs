module DuDuHoX.Console.World where

import           Data.List              ((\\))
import           DuDuHoX.Console.Core
import           DuDuHoX.Util.Bresenham
import           DuDuHoX.World

data ConsoleWorld =
    ConsoleWorld {
        viewer :: ConsoleObject,
        seen :: [ConsoleObject],
        fog :: [ConsoleObject],
        unseen :: [ConsoleObject],
        world :: World
    }

mkWorld :: World -> ConsoleWorld
mkWorld w = updateVision
    ConsoleWorld {
        viewer = player,
        seen = [],
        fog = [],
        unseen = exit : walls ++ floors,
        world = w
    }
    where
        player = mkPlayer $ worldPlayer w
        exit = mkExit $ worldExit w
        walls = map mkWall $ worldWalls w
        floors = map mkFloor $ worldFloors w

updateWorld :: ConsoleWorld -> World -> ConsoleWorld
updateWorld cw w = updateVision $ cw { viewer = player, world = w }
    where player = mkPlayer $ worldPlayer w

mkPlayer :: WorldPlayer -> ConsoleObject
mkPlayer p =
    ConsoleObject {
        graphic = '@',
        translucid = True,
        position = mkPosition $ playerPosition p
    }

mkExit :: WorldExit -> ConsoleObject
mkExit e =
    ConsoleObject {
        graphic = '!',
        translucid = True,
        position = mkPosition $ exitPosition e
    }

mkWall :: WorldWall -> ConsoleObject
mkWall w =
    ConsoleObject {
        graphic = '#',
        translucid = False,
        position = mkPosition $ wallPosition w
    }

mkFloor :: WorldFloor -> ConsoleObject
mkFloor f =
    ConsoleObject {
        graphic = '.',
        translucid = True,
        position = mkPosition $ floorPosition f
    }

mkPosition :: WorldPosition -> ConsolePosition
mkPosition p =
    ConsolePosition {
        consoleX = x p,
        consoleY = y p
    }

drawWorld :: ConsoleWorld -> IO ()
drawWorld w = do
    inFog
    mapM_ draw $ fog w

    inSight
    mapM_ draw $ seen w
    draw $ viewer w

updateVision :: ConsoleWorld -> ConsoleWorld
updateVision w =
    w {
        seen = seen',
        fog = fog',
        unseen = unseen'
    }
    where
        seen' = filter (isVisible w . position) $ allObjects w
        fog' = filter (not . isVisible w . position) $ seen w ++ fog w
        unseen' = allObjects w \\ (seen' ++ fog')

isVisible :: ConsoleWorld -> ConsolePosition -> Bool
isVisible w p = not $ hasSolidBetween w (position $ viewer w) p

hasSolidBetween :: ConsoleWorld -> ConsolePosition -> ConsolePosition -> Bool
hasSolidBetween w v o = any (not . translucid) $ objectsBetween w v o

objectsBetween :: ConsoleWorld -> ConsolePosition -> ConsolePosition -> [ConsoleObject]
objectsBetween w v o = filter (flip elem path . position) $ allObjects w
    where path = pathBetween v o

pathBetween :: ConsolePosition -> ConsolePosition -> [ConsolePosition]
pathBetween a b = map (uncurry ConsolePosition) $ digitalLine (consoleX a, consoleY a) (consoleX b, consoleY b)

allObjects :: ConsoleWorld -> [ConsoleObject]
allObjects w = seen w ++ fog w ++ unseen w
