module DuDuHoX.World.Types where

data WorldPosition =
    WorldPosition {
        worldX :: Int,
        worldY :: Int
    }
    deriving (Eq, Show)

data WorldPlayer =
    WorldPlayer {
        playerPosition :: WorldPosition
    }

data WorldWall =
    WorldWall {
        wallPosition :: WorldPosition
    }

data WorldExit =
    WorldExit {
        exitPosition :: WorldPosition
    }

data WorldFloor =
    WorldFloor {
        floorPosition :: WorldPosition
    }

data World =
    World {
        worldPlayer :: WorldPlayer,
        worldWalls :: [WorldWall],
        worldFloors :: [WorldFloor],
        worldExit :: WorldExit
    }

class WorldObject a where
    worldPosition :: a -> WorldPosition

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
