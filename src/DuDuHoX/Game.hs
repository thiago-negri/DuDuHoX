module DuDuHoX.Game where

import DuDuHoX.World

data GameInput =
    MoveUp |
    MoveDown |
    MoveLeft |
    MoveRight |
    Quit

handleInput :: World -> GameInput -> Maybe World
handleInput w a = case a of
    Quit -> Nothing
    MoveUp -> Just . movePlayer $ WorldPosition 0 (-1)
    MoveDown -> Just . movePlayer $ WorldPosition 0 1
    MoveLeft -> Just . movePlayer $ WorldPosition (-1) 0
    MoveRight -> Just . movePlayer $ WorldPosition 1 0
    where
        movePlayer mp = w{worldPlayer = p{playerPosition = tryMove mp}}
        p = worldPlayer w
        pp = playerPosition p
        tryMove mp = let
                        np = pp |+| mp
                        o = filter (\wo -> wallPosition wo == np) $ worldWalls w in
                            if null o
                                then np
                                else pp
