module DuDuHoX.World.Builder (parseWorld) where

import           Control.Monad.Trans.State.Lazy
import           DuDuHoX.World.Types

data WorldBuilder =
    WorldBuilder {
        builderPlayer :: Maybe WorldPlayer,
        builderWalls :: [WorldWall],
        builderFloors :: [WorldFloor],
        builderExit :: Maybe WorldExit
    }

parseWorld :: [String] -> Maybe World
parseWorld worldLines = do
    player <- builderPlayer builder
    exit <- builderExit builder
    return World {
        worldPlayer = player,
        worldWalls = builderWalls builder,
        worldFloors = builderFloors builder,
        worldExit = exit
    }
    where builder = execState (parseWorldInfo worldLines) emptyBuilder

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
addPlayer pos = modify $ \builder ->
    builder {
        builderPlayer = Just $ WorldPlayer pos
    }

addFloor :: WorldPosition -> State WorldBuilder ()
addFloor pos = modify $ \builder ->
    builder {
        builderFloors = WorldFloor pos : builderFloors builder
    }

addExit :: WorldPosition -> State WorldBuilder ()
addExit pos = modify $ \builder ->
    builder {
        builderExit = Just $ WorldExit pos
    }

parseWorldInfo :: [String] -> State WorldBuilder ()
parseWorldInfo worldLines = threadState (indexated worldLines) parseWorldLineInfo

threadState :: [a] -> (a -> State s ()) -> State s ()
threadState [] _ = return ()
threadState (x:xs) f = f x >> threadState xs f

parseWorldLineInfo :: (Int, String) -> State WorldBuilder ()
parseWorldLineInfo (y, worldLine) = threadState (indexated worldLine) (parseWorldCharInfo y)

parseWorldCharInfo :: Int -> (Int, Char) -> State WorldBuilder ()
parseWorldCharInfo y (x, c) =
    case c of
        '#' -> addWall pos
        '@' -> addPlayer pos >> addFloor pos
        '.' -> addFloor pos
        '!' -> addExit pos
        _ -> return ()
    where pos = WorldPosition x y

indexated :: [a] -> [(Int, a)]
indexated = zip [0..]
