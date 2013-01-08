module DuDuHoX.OpenGL.Data where

import           Data.IORef

import qualified Graphics.Rendering.OpenGL as GL

import           DuDuHoX.Game
import           DuDuHoX.OpenGL.Animation
import           DuDuHoX.World.Types

data FPSCounter = FPSCounter {
    currentFPS :: Double -- ^ Current FPS counter
   ,timeSinceLastUpdate :: Double -- ^ Time elapsed since last FPS counter update in seconds 
}

data GLPlayer = GLPlayer {
    delta :: (GL.GLfloat, GL.GLfloat) -- ^ Player delta
   ,animation :: Animation
}

data GLState = Accept | Render deriving (Eq)

data DuDuHoXGLTextures = DuDuHoXGLTextures {
    backgroundTex :: GL.TextureObject -- ^ Texture used for background
}

data DuDuHoXGLContext = DuDuHoXGLContext {
    dirty :: IORef Bool -- ^ Should repaint the screen?
   ,userInput :: IORef (Maybe GameInput) -- ^ User input
   ,quit :: IORef Bool -- ^ Should quit the game?
   ,world :: IORef VisibleWorld -- ^ The current state of the game
   ,textures :: IORef DuDuHoXGLTextures -- ^ Textures used in game
   ,player :: IORef GLPlayer
   ,state :: IORef GLState
   ,fpsCounter :: IORef FPSCounter
}

mkContext :: IORef VisibleWorld -> IORef GLPlayer -> IORef DuDuHoXGLTextures -> IO DuDuHoXGLContext
mkContext world' player' textures' = do
    dirty' <- newIORef True
    quit' <- newIORef False
    userInput' <- newIORef Nothing
    state' <- newIORef Accept
    fpsCounter' <- newIORef $ FPSCounter 0 1
    return DuDuHoXGLContext {
        dirty = dirty',
        userInput = userInput',
        quit = quit',
        world = world',
        textures = textures',
        player = player',
        state = state',
        fpsCounter = fpsCounter'
    }
