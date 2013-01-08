module DuDuHoX.World.Base where

import           DuDuHoX.Game
import           DuDuHoX.World.Types

(|+|) :: WorldPosition -> WorldPosition -> WorldPosition
a |+| b = WorldPosition (worldX a + worldX b) (worldY a + worldY b)

(|-|) :: WorldPosition -> WorldPosition -> WorldPosition
a |-| b = WorldPosition (worldX a - worldX b) (worldY a - worldY b)

movePlayer :: World -> MoveDirection -> World
movePlayer world move = result
    where
        result = if validNewPosition then newWorld else world
        validNewPosition = world `isWalkableAt` newPosition
        newPosition = playerPosition player |+| deltaM move
        player = worldPlayer world
        newWorld = world{worldPlayer=player{playerPosition=newPosition}}

isWalkableAt :: World -> WorldPosition -> Bool
world `isWalkableAt` pos = exitPosition (worldExit world) == pos || world `hasFloorAt` pos

hasFloorAt :: World -> WorldPosition -> Bool
world `hasFloorAt` pos = any ((pos ==) . floorPosition) floors
    where floors = worldFloors world

deltaM :: MoveDirection -> WorldPosition
deltaM MoveUp = WorldPosition 0 (-1)
deltaM MoveDown = WorldPosition 0 1
deltaM MoveLeft = WorldPosition (-1) 0
deltaM MoveRight = WorldPosition 1 0

won :: World -> Bool
won w = playerPosition (worldPlayer w) == exitPosition (worldExit w)
