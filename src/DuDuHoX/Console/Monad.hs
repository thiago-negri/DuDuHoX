{-# LANGUAGE RecordWildCards #-}
module DuDuHoX.Console.Monad
    (DConsole
    ,DuDuHoXConsole(..)
    ,ConsoleVisibility(..)
    ,runConsole
    )
where

import           DuDuHoX.Console.Core
import           DuDuHoX.Console.Types

data ConsoleVisibility = InSight | Fog

data DuDuHoXConsole =
    ClearScreen { next :: DuDuHoXConsole }
  | DrawObjects { visibility :: ConsoleVisibility, objects :: [ConsoleObject], next :: DuDuHoXConsole }
  | ClearBox { corner :: ConsolePosition, height :: Int, width :: Int, next :: DuDuHoXConsole }
  | DrawBox { corner :: ConsolePosition, height :: Int, width :: Int, next :: DuDuHoXConsole }
  | DrawText { corner :: ConsolePosition, text :: String, next :: DuDuHoXConsole }
  | End

type DConsole = DuDuHoXConsole -> DuDuHoXConsole

runConsole :: DConsole -> IO ()
runConsole = runConsole' . ($ End)

runConsole' :: DuDuHoXConsole -> IO ()
runConsole' x =
    case x of
        ClearScreen{..} -> clearConsole >> runConsole' next
        DrawObjects{..} -> (case visibility of
                                InSight -> inSight
                                Fog     -> inFog) >> mapM_ draw objects >> runConsole' next
        ClearBox{..}    -> clearBox corner height width >> runConsole' next
        DrawBox{..}     -> drawBox corner height width >> runConsole' next
        DrawText{..}    -> drawText corner text >> runConsole' next
        End             -> return ()
