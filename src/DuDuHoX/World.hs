module DuDuHoX.World where

import DuDuHoX.Game
import DuDuHoX.Util.Bresenham

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

data World =
    World {
        worldPlayer :: WorldPlayer,
        worldWalls :: [WorldWall],
        worldExit :: WorldExit
    }

data WorldUpdate =
    PlayerMove {
        moveDirection :: MoveDirection
    }

(|+|) :: WorldPosition -> WorldPosition -> WorldPosition
a |+| b = WorldPosition (x a + x b) (y a + y b)

runUpdate :: World -> WorldUpdate -> (World, Maybe WorldUpdate)
runUpdate world update@(PlayerMove move) = result
    where
        result = if validNewPosition then (newWorld, Just update) else (world, Nothing)
        validNewPosition = not $ world `hasAnyWallAt` newPosition
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

hasAnyWallAt :: World -> WorldPosition -> Bool
world `hasAnyWallAt` position = any ((position ==) . wallPosition) walls
    where walls = worldWalls world

hasAnyWallBetween :: World -> WorldPosition -> WorldPosition -> Bool
hasAnyWallBetween world a b = any (hasAnyWallAt world) path
    where path = straightPath a b

straightPath :: WorldPosition -> WorldPosition -> [WorldPosition]
straightPath a b = map (uncurry WorldPosition) $ digitalLine (x a, y a) (x b, y b)

delta :: MoveDirection -> WorldPosition
delta MoveUp = WorldPosition 0 (-1)
delta MoveDown = WorldPosition 0 1
delta MoveLeft = WorldPosition (-1) 0
delta MoveRight = WorldPosition 1 0
delta MoveUpLeft = delta MoveUp |+| delta MoveLeft
delta MoveUpRight = delta MoveUp |+| delta MoveRight
delta MoveDownLeft = delta MoveDown |+| delta MoveLeft
delta MoveDownRight = delta MoveDown |+| delta MoveRight
