module DuDuHoX.World where

import DuDuHoX.Game

data WorldPosition =
    WorldPosition {
        x :: Int,
        y :: Int
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

data WorldUpdate =
    PlayerMove {
        moveDirection :: MoveDirection
    }

(|+|) :: WorldPosition -> WorldPosition -> WorldPosition
a |+| b = WorldPosition (x a + x b) (y a + y b)

runUpdate :: World -> WorldUpdate -> World
runUpdate world (PlayerMove move) = result
    where
        result = if validNewPosition then newWorld else world
        validNewPosition = world `isWalkableAt` newPosition
        newPosition = playerPosition player |+| delta move
        player = worldPlayer world
        newWorld = world{worldPlayer=player{playerPosition=newPosition}}

class WorldObject a where
    worldPosition :: a -> WorldPosition

instance WorldObject WorldWall where
    worldPosition = wallPosition

instance WorldObject WorldExit where
    worldPosition = exitPosition

instance WorldObject WorldPlayer where
    worldPosition = playerPosition

isWalkableAt :: World -> WorldPosition -> Bool
world `isWalkableAt` position = exitPosition (worldExit world) == position || world `hasAnyFloorAt` position

hasAnyFloorAt :: World -> WorldPosition -> Bool
world `hasAnyFloorAt` position = any ((position ==) . floorPosition) floors
    where floors = worldFloors world
    
delta :: MoveDirection -> WorldPosition
delta MoveUp = WorldPosition 0 (-1)
delta MoveDown = WorldPosition 0 1
delta MoveLeft = WorldPosition (-1) 0
delta MoveRight = WorldPosition 1 0

won :: World -> Bool
won w = playerPosition (worldPlayer w) == exitPosition (worldExit w)
