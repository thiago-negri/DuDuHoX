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
        builderExit :: Maybe WorldExit
    }

parseWorld :: [String] -> Maybe World
parseWorld worldLines = do 
    player <- mPlayer
    exit <- mExit
    return World{worldPlayer = player, worldWalls = walls, worldExit = exit}
    where 
        builder = parseWorldInfo worldLines
        mPlayer = builderPlayer builder
        mExit = builderExit builder
        walls = builderWalls builder

joinBuilders :: WorldBuilder -> WorldBuilder -> WorldBuilder
joinBuilders a b = WorldBuilder { builderPlayer = newPlayer, builderWalls = newWalls, builderExit = newExit }
    where
        newPlayer = joinOn builderPlayer
        newExit = joinOn builderExit
        newWalls = joinOn builderWalls
        joinOn f = (mplus `on` f) a b

emptyBuilder :: WorldBuilder
emptyBuilder = WorldBuilder { builderPlayer = Nothing, builderWalls = [], builderExit = Nothing }

buildWall :: WorldPosition -> WorldBuilder
buildWall position = emptyBuilder { builderWalls = [WorldWall position] }

buildPlayer :: WorldPosition -> WorldBuilder
buildPlayer position = emptyBuilder { builderPlayer = Just $ WorldPlayer position }

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
    '@' -> buildPlayer position
    '!' -> buildExit position
    _ -> emptyBuilder
    where position = WorldPosition x y

indexated :: [a] -> [(Int, a)]
indexated = zip [0..]
