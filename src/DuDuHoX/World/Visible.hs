module DuDuHoX.World.Visible where

import           DuDuHoX.World.Types
import           Data.List              ((\\))
import           DuDuHoX.Util.Bresenham

data VisibleWorld =
    VisibleWorld {
        viewer :: VisibleObject,
        seen :: [VisibleObject],
        fog :: [VisibleObject],
        unseen :: [VisibleObject],
        vWorld :: World
    }

data VisibleObject = VisibleObject { oType :: WorldObjectType, position :: WorldPosition } deriving Eq
data WorldObjectType = Wall | Floor | Exit | Player deriving Eq

mkVisWorld :: World -> VisibleWorld
mkVisWorld w = updateVision
    VisibleWorld {
        viewer = player,
        seen = [],
        fog = [],
        unseen = exit : walls ++ floors,
        vWorld = w
    }
    where
        player = mkPlayer $ worldPlayer w
        exit = mkExit $ worldExit w
        walls = map mkWall $ worldWalls w
        floors = map mkFloor $ worldFloors w

updateWorld :: VisibleWorld -> World -> VisibleWorld
updateWorld cw w = updateVision $ cw { viewer = player, vWorld = w }
    where player = mkPlayer $ worldPlayer w

mkPlayer :: WorldPlayer -> VisibleObject
mkPlayer p = VisibleObject Player $ playerPosition p

mkExit :: WorldExit -> VisibleObject
mkExit e = VisibleObject Exit $ exitPosition e

mkWall :: WorldWall -> VisibleObject
mkWall w = VisibleObject Wall $ wallPosition w

mkFloor :: WorldFloor -> VisibleObject
mkFloor f = VisibleObject Floor $ floorPosition f

updateVision :: VisibleWorld -> VisibleWorld
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

isVisible :: VisibleWorld -> WorldPosition -> Bool
isVisible w p = not $ hasSolidBetween w (position $ viewer w) p

hasSolidBetween :: VisibleWorld -> WorldPosition -> WorldPosition -> Bool
hasSolidBetween w v o = any (not . translucid) $ objectsBetween w v o

objectsBetween :: VisibleWorld -> WorldPosition -> WorldPosition -> [VisibleObject]
objectsBetween w v o = filter (flip elem path . position) $ allObjects w
    where path = pathBetween v o

pathBetween :: WorldPosition -> WorldPosition -> [WorldPosition]
pathBetween a b = map (uncurry WorldPosition) $ digitalLine (worldX a, worldY a) (worldX b, worldY b)

allObjects :: VisibleWorld -> [VisibleObject]
allObjects w = seen w ++ fog w ++ unseen w

translucid :: VisibleObject -> Bool
translucid v = case oType v of
    Wall -> False
    Player -> True
    Exit -> True
    Floor -> True

filterFloorWallExit :: [VisibleObject] -> ([VisibleObject], [VisibleObject], [VisibleObject])
filterFloorWallExit = go [] [] []
    where go f w e [] = (f, w, e)
          go f w e (x:xs) = case oType x of
                                Floor -> let f' = (x:f) in f' `seq` go f' w e xs
                                Wall -> let w' = (x:w) in w' `seq` go f w' e xs
                                Exit -> let e' = (x:e) in e' `seq`go f w e' xs
                                Player -> go f w e xs
