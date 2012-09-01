module DuDuHoX.Console.Types where

data ConsoleObject =
    ConsoleObject {
        graphic :: Char,
        translucid :: Bool,
        position :: ConsolePosition
    }
    deriving (Eq)

data ConsolePosition =
    ConsolePosition {
        consoleX :: Int,
        consoleY :: Int
    }
    deriving (Eq)

cpPlus :: ConsolePosition -> ConsolePosition -> ConsolePosition
cpPlus a b = ConsolePosition (consoleX a + consoleX b) (consoleY a + consoleY b)

cpMinus :: ConsolePosition -> ConsolePosition -> ConsolePosition
cpMinus a b = ConsolePosition (consoleX b - consoleX a) (consoleY b - consoleY a)