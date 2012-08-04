module DuDuHoX.Util.Bresenham (digitalLine) where

{-
Adaptado de:
http://roguebasin.roguelikedevelopment.org/index.php/Bresenham's_Line_Algorithm
-}

type Point = (Int, Int)
type Line = [Point]

type Step = Signal -> Point -> Point

type P = Int
type Q = Int
type EPS = Int

data Signal = Zero | One
type QuadraticWave = [Signal]

type BalancedWordStrategy = (QuadraticWave, Step)

asInt :: Signal -> Int
asInt Zero = 0
asInt One = 1

balancedWord :: P -> Q -> EPS -> QuadraticWave
balancedWord p q eps = signal : balancedWord p q newContrivedEps
    where
        (signal, newContrivedEps) = if newEps < q then (Zero, newEps) else (One, newEps - q)
        newEps = eps + p

getStrategy :: Point -> Point -> BalancedWordStrategy
getStrategy (x0, y0) (x1, y1) = (balance, step)
    where
        (p, q, step) | abs dx > abs dy = (abs dy, abs dx, horizontalStep)
                     | otherwise       = (abs dx, abs dy, verticalStep)
        balance = balancedWord p q 0
        (dx, dy) = (x1 - x0, y1 - y0)
        horizontalStep signal (x, y) = (x + signum dx,                y + signum dy * asInt signal)
        verticalStep   signal (x, y) = (x + signum dx * asInt signal, y + signum dy)

digitalLine :: Point -> Point -> Line
digitalLine a b = takeWhile (/= b) $ walk balance a
  where
      (balance, step) = getStrategy a b
      walk (signal:signals) point = let point' = (step signal point) in point' : walk signals point'
