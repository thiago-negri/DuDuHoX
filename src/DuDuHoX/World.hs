module DuDuHoX.World where

import DuDuHoX.Game

data WorldPosition =
    WorldPosition {
        x :: Int,
        y :: Int
    }
    deriving (Eq)

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
runUpdate world update@(PlayerMove move) = (maybeNewWorld, maybeWorldUpdate)
    where
        newPosition = playerPosition player |+| delta move
        delta MoveUp = WorldPosition 0 (-1)
        delta MoveDown = WorldPosition 0 1
        delta MoveLeft = WorldPosition (-1) 0
        delta MoveRight = WorldPosition 1 0
        validNewPosition = not $ any ((newPosition ==) . wallPosition) walls
        walls = worldWalls world
        player = worldPlayer world
        maybeWorldUpdate = 
            if validNewPosition
                then Just update
                else Nothing
        maybeNewWorld =
            if validNewPosition
                then newWorld
                else world
        newWorld = world{worldPlayer=player{playerPosition=newPosition}}

class WorldObject a where
    worldPosition :: a -> WorldPosition

instance WorldObject WorldWall where
    worldPosition = wallPosition

instance WorldObject WorldExit where
    worldPosition = exitPosition

instance WorldObject WorldPlayer where
    worldPosition = playerPosition

distance :: WorldPosition -> WorldPosition -> Int
distance a b = abs (x a - x b) + abs (y a - y b)
