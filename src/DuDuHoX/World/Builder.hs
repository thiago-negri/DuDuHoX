module DuDuHoX.World.Builder (
    parseWorld
    )
where

import           Control.Monad.Trans.State.Lazy
import           DuDuHoX.World

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
        builder = execState (parseWorldInfo worldLines) emptyBuilder
        mPlayer = builderPlayer builder
        mExit = builderExit builder
        walls = builderWalls builder
        floors = builderFloors builder

emptyBuilder :: WorldBuilder
emptyBuilder =
    WorldBuilder {
        builderPlayer = Nothing,
        builderWalls = [],
        builderFloors = [],
        builderExit = Nothing
    }

addWall :: WorldPosition -> State WorldBuilder ()
addWall p = modify $ \builder ->
    builder {
        builderWalls = WorldWall p : builderWalls builder
    }

addPlayer :: WorldPosition -> State WorldBuilder ()
addPlayer position = modify $ \builder ->
    builder {
        builderPlayer = Just $ WorldPlayer position
    }

addFloor :: WorldPosition -> State WorldBuilder ()
addFloor position = modify $ \builder ->
    builder {
        builderFloors = WorldFloor position : builderFloors builder
    }

addExit :: WorldPosition -> State WorldBuilder ()
addExit position = modify $ \builder ->
    builder {
        builderExit = Just $ WorldExit position
    }

parseWorldInfo :: [String] -> State WorldBuilder ()
parseWorldInfo worldLines = threadState indexatedWorldLines f
    where
        f = parseWorldLineInfo
        indexatedWorldLines = indexated worldLines

threadState :: [a] -> (a -> State s ()) -> State s ()
threadState [] _ = return ()
threadState (x:xs) f = f x >> threadState xs f

parseWorldLineInfo :: (Int, String) -> State WorldBuilder ()
parseWorldLineInfo (y, worldLine) = threadState indexatedWorldLine f
    where
        f = parseWorldCharInfo y
        indexatedWorldLine = indexated worldLine

parseWorldCharInfo :: Int -> (Int, Char) -> State WorldBuilder ()
parseWorldCharInfo y (x, c) = case c of
    '#' -> addWall position
    '@' -> addPlayer position >> addFloor position
    '.' -> addFloor position
    '!' -> addExit position
    _ -> return ()
    where position = WorldPosition x y

indexated :: [a] -> [(Int, a)]
indexated = zip [0..]
