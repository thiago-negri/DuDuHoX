module DuDuHoX.World where

import Control.Monad
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

parseWorld :: [String] -> Maybe World
parseWorld worldLines = do 
    player <- mPlayer
    exit <- mExit
    return World{worldPlayer = player, worldWalls = walls, worldExit = exit}
    where 
        (mPlayer, mExit, walls) = parseWorldInfo worldLines

parseWorldInfo :: [String] -> (Maybe WorldPlayer, Maybe WorldExit, [WorldWall])
parseWorldInfo worldLines = go (Nothing, Nothing, []) indexatedWorldLines
    where
        go acc [] = acc
        go (p, e, w) (line:etc) =
            let (p', e', w') = parseWorldLineInfo line in
                go (p `mplus` p', e `mplus` e', w ++ w') etc
        indexatedWorldLines = indexated . map indexated $ worldLines

parseWorldLineInfo :: (Int, [(Int, Char)]) -> (Maybe WorldPlayer, Maybe WorldExit, [WorldWall])
parseWorldLineInfo worldLine = go (Nothing, Nothing, []) $ snd worldLine
    where
        go acc [] = acc
        go acc@(p, e, w) ((x, c):etc) = 
            let position = WorldPosition x y in
                case c of
                    '#' -> go (p, e, WorldWall{wallPosition = position} : w) etc
                    '@' -> go (p `mplus` Just WorldPlayer{playerPosition = position}, e, w) etc
                    '!' -> go (p, e `mplus` Just WorldExit{exitPosition = position}, w) etc
                    _ -> go acc etc
        y = fst worldLine

indexated :: [a] -> [(Int, a)]
indexated = zip [0..]

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
