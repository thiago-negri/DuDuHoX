module DuDuHoX.World.Builder (
    parseWorld
    ) 
where

import Control.Monad (mplus)
import Data.Function (on)
import DuDuHoX.World

data WorldBuilder =
    WorldBuilder {
        builderPlayer :: Maybe WorldPlayer,
        builderWalls :: [WorldWall],
        builderFloors :: [WorldFloor],
        builderExit :: Maybe WorldExit
    }

parseWorld :: [String] -> Maybe World
parseWorld worldLines = do 
    player <- mPlayer
    exit <- mExit
    return World{worldPlayer = player, worldWalls = walls, worldFloors = floors, worldExit = exit}
    where 
        builder = parseWorldInfo worldLines
        mPlayer = builderPlayer builder
        mExit = builderExit builder
        walls = builderWalls builder
        floors = builderFloors builder

joinBuilders :: WorldBuilder -> WorldBuilder -> WorldBuilder
joinBuilders a b = 
    WorldBuilder { 
        builderPlayer = newPlayer, 
        builderWalls = newWalls, 
        builderFloors = newFloors, 
        builderExit = newExit 
    }
    where
        newPlayer = joinOn builderPlayer
        newExit = joinOn builderExit
        newWalls = joinOn builderWalls
        newFloors = joinOn builderFloors
        joinOn f = (mplus `on` f) a b

emptyBuilder :: WorldBuilder
emptyBuilder = WorldBuilder { builderPlayer = Nothing, builderWalls = [], builderFloors = [], builderExit = Nothing }

buildWall :: WorldPosition -> WorldBuilder
buildWall position = emptyBuilder { builderWalls = [WorldWall position] }

buildPlayer :: WorldPosition -> WorldBuilder
buildPlayer position = emptyBuilder { builderPlayer = Just $ WorldPlayer position }

buildFloor :: WorldPosition -> WorldBuilder
buildFloor position = emptyBuilder { builderFloors = [WorldFloor position] }

buildExit :: WorldPosition -> WorldBuilder
buildExit position = emptyBuilder { builderExit = Just $ WorldExit position }

parseWorldInfo :: [String] -> WorldBuilder
parseWorldInfo worldLines = foldl f emptyBuilder indexatedWorldLines
    where
        f a = joinBuilders a . parseWorldLineInfo  
        indexatedWorldLines = indexated worldLines

parseWorldLineInfo :: (Int, String) -> WorldBuilder
parseWorldLineInfo (y, worldLine) = foldl f emptyBuilder indexatedWorldLine
    where
        f a = joinBuilders a . parseWorldCharInfo y
        indexatedWorldLine = indexated worldLine

parseWorldCharInfo :: Int -> (Int, Char) -> WorldBuilder
parseWorldCharInfo y (x, c) = case c of
    '#' -> buildWall position
    '@' -> buildPlayer position `joinBuilders` buildFloor position
    '.' -> buildFloor position
    '!' -> buildExit position
    _ -> emptyBuilder
    where position = WorldPosition x y

indexated :: [a] -> [(Int, a)]
indexated = zip [0..]
