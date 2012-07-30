module DuDuHoX.Game where

import DuDuHoX.World

data GameInput =
    MoveUp |
    MoveDown |
    MoveLeft |
    MoveRight |
    Quit

gameLoop :: World -> IO ()
gameLoop world = do
    drawWorld world
    input <- getInput
    handleInput world input

getInput :: IO (Maybe GameInput)
getInput = do
    char <- getChar
    return $ parseGameInput char

parseGameInput :: Char -> Maybe GameInput
parseGameInput 'w' = Just MoveUp
parseGameInput 's' = Just MoveDown
parseGameInput 'a' = Just MoveLeft
parseGameInput 'd' = Just MoveRight
parseGameInput 'q' = Just Quit
parseGameInput _ = Nothing

drawWorld :: World -> IO ()
drawWorld = undefined

handleInput :: World -> Maybe GameInput -> IO ()
handleInput w Nothing = gameLoop w
handleInput w (Just a) = case a of
    Quit -> return ()
    MoveUp -> movePlayer $ WorldPosition 0 (-1)
    MoveDown -> movePlayer $ WorldPosition 0 1
    MoveLeft -> movePlayer $ WorldPosition (-1) 0
    MoveRight -> movePlayer $ WorldPosition 0 1
    where
        movePlayer mp = gameLoop w{worldPlayer = p{playerPosition = tryMove mp}}
        p = worldPlayer w
        pp = playerPosition p
        tryMove mp = let
                        np = pp |+| mp
                        o = filter (\wo -> wallPosition wo == np) $ worldWalls w in
                            if null o
                                then np
                                else pp
