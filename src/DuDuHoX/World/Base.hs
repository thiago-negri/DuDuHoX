module DuDuHoX.World.Base where

import           DuDuHoX.Game
import           DuDuHoX.World.Types

(|+|) :: WorldPosition -> WorldPosition -> WorldPosition
a |+| b = WorldPosition (worldX a + worldX b) (worldY a + worldY b)

movePlayer :: World -> MoveDirection -> World
movePlayer world move = result
    where
        result = if validNewPosition then newWorld else world
        validNewPosition = world `isWalkableAt` newPosition
        newPosition = playerPosition player |+| delta move
        player = worldPlayer world
        newWorld = world{worldPlayer=player{playerPosition=newPosition}}

instance WorldObject WorldWall where
    worldPosition = wallPosition

instance WorldObject WorldExit where
    worldPosition = exitPosition

instance WorldObject WorldPlayer where
    worldPosition = playerPosition

instance WorldObject WorldFloor where
    worldPosition = floorPosition

isWalkableAt :: World -> WorldPosition -> Bool
world `isWalkableAt` position = exitPosition (worldExit world) == position || world `hasFloorAt` position

hasFloorAt :: World -> WorldPosition -> Bool
world `hasFloorAt` position = any ((position ==) . floorPosition) floors
    where floors = worldFloors world

delta :: MoveDirection -> WorldPosition
delta MoveUp = WorldPosition 0 (-1)
delta MoveDown = WorldPosition 0 1
delta MoveLeft = WorldPosition (-1) 0
delta MoveRight = WorldPosition 1 0

won :: World -> Bool
won w = playerPosition (worldPlayer w) == exitPosition (worldExit w)
