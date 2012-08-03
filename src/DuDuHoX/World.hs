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

hasAnyWallAt :: World -> WorldPosition -> Bool
world `hasAnyWallAt` position = any ((position ==) . wallPosition) walls
    where walls = worldWalls world

hasAnyWallBetween :: World -> WorldPosition -> WorldPosition -> Bool
hasAnyWallBetween world a b = any (hasAnyWallAt world) path
    where path = straightPath a b

straightPath :: WorldPosition -> WorldPosition -> [WorldPosition]
straightPath a b | a == b    = []
                 | otherwise = a : straightPath a' b
    where a' = a |+| moveDelta a b

moveDelta :: WorldPosition -> WorldPosition -> WorldPosition
moveDelta a b = delta . direction $ diff a b

delta :: MoveDirection -> WorldPosition
delta MoveUp = WorldPosition 0 (-1)
delta MoveDown = WorldPosition 0 1
delta MoveLeft = WorldPosition (-1) 0
delta MoveRight = WorldPosition 1 0
delta MoveUpLeft = delta MoveUp |+| delta MoveLeft
delta MoveUpRight = delta MoveUp |+| delta MoveRight
delta MoveDownLeft = delta MoveDown |+| delta MoveLeft
delta MoveDownRight = delta MoveDown |+| delta MoveRight

diff :: WorldPosition -> WorldPosition -> (Int, Int)
diff a b = (x b - x a, y b - y a)

direction :: (Int, Int) -> MoveDirection
direction (x, y) | x' > y'   = horizontal
                 | x' == y'  = diagonal
                 | otherwise = vertical
    where
        x' = abs x
        y' = abs y
        horizontal = if x < 0 then MoveLeft else MoveRight
        diagonal = if x < 0 then if y < 0 then MoveUpLeft
                                          else MoveDownLeft
                            else if y < 0 then MoveUpRight
                                          else MoveDownRight
        vertical = if y < 0 then MoveUp else MoveDown