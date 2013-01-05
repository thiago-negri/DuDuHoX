module DuDuHoX.Console.Types where

import           Control.Monad.Free
import           DuDuHoX.Game
import           DuDuHoX.World.Types

data ConsolePosition =
    ConsolePosition {
        consoleX :: Int,
        consoleY :: Int
    }
    deriving (Eq)

cpPlus :: ConsolePosition -> ConsolePosition -> ConsolePosition
cpPlus a b = ConsolePosition (consoleX a + consoleX b) (consoleY a + consoleY b)

cpMinus :: ConsolePosition -> ConsolePosition -> ConsolePosition
cpMinus a b = ConsolePosition (consoleX a - consoleX b) (consoleY a - consoleY b)

data ConsoleVisibility = InSight | Fog

data DuDuHoXConsole next =
    ClearScreen { next :: next }
  | DrawObjects { visibility :: ConsoleVisibility, objects :: [VisibleObject], next :: next }
  | ClearBox { corner :: ConsolePosition, height :: Int, width :: Int, next :: next }
  | DrawBox { corner :: ConsolePosition, height :: Int, width :: Int, next :: next }
  | DrawText { corner :: ConsolePosition, text :: String, next :: next }
  | Pause { next :: next }
  | GetUserInput { nextGI :: GameInput -> next }
  | End

instance Functor DuDuHoXConsole where
    fmap f (ClearScreen n) = ClearScreen (f n)
    fmap f (DrawObjects v o n) = DrawObjects v o (f n)
    fmap f (ClearBox c h w n) = ClearBox c h w (f n)
    fmap f (DrawBox c h w n) = DrawBox c h w (f n)
    fmap f (DrawText c t n) = DrawText c t (f n)
    fmap f (Pause n) = Pause (f n)
    fmap f (GetUserInput n) = GetUserInput (f . n)
    fmap _ End = End

type DuDuHoXConsoleM a = Free DuDuHoXConsole a

clearScreen' :: DuDuHoXConsoleM ()
clearScreen' = liftF (ClearScreen ())

drawObjects' :: ConsoleVisibility -> [VisibleObject] -> DuDuHoXConsoleM ()
drawObjects' v o = liftF (DrawObjects v o ())

clearBox' :: ConsolePosition -> Int -> Int -> DuDuHoXConsoleM ()
clearBox' c h w = liftF (ClearBox c h w ())

drawBox' :: ConsolePosition -> Int -> Int -> DuDuHoXConsoleM ()
drawBox' c h w = liftF (DrawBox c h w ())

drawText' :: ConsolePosition -> String -> DuDuHoXConsoleM ()
drawText' c t = liftF (DrawText c t ())

pause' :: DuDuHoXConsoleM ()
pause' = liftF (Pause ())

getUserInput' :: DuDuHoXConsoleM GameInput
getUserInput' = liftF (GetUserInput id)

end' :: DuDuHoXConsoleM ()
end' = liftF End
