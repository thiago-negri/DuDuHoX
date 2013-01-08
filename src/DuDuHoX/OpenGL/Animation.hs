module DuDuHoX.OpenGL.Animation where

import qualified Graphics.Rendering.OpenGL as GL

newtype Animation = Animation [Frame]
type Frame = (Sprite, Duration)
type Duration = Time
type Time = Double -- ^ Seconds
type Sprite = (GL.TextureObject, Dimension)
type Dimension = (Width, Height)
type Width = Int
type Height = Int

animate :: Animation -> Time -> Animation
animate (Animation []) _ = error "animate: empty list"
animate (Animation ((s, d) : ss)) t | t < d     = Animation ((s, d - t) : ss)
                                    | otherwise = animate (Animation ss) (t - d)

createAnimation :: [Frame] -> Animation
createAnimation [] = error "createAnimation: empty list"
createAnimation a = Animation $ cycle a

currentSprite :: Animation -> Sprite
currentSprite (Animation []) = error "currentSprite: empty list"
currentSprite (Animation ((s, _):_)) = s
