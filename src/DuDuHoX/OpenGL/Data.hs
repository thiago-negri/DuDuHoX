module DuDuHoX.OpenGL.Data where

import           Data.IORef

import qualified Graphics.Rendering.OpenGL as GL

import           DuDuHoX.Game
import           DuDuHoX.World.Types

data GLPlayer = GLPlayer {
    delta :: (GL.GLfloat, GL.GLfloat) -- ^ Player delta
}

data GLState = Accept | Render deriving (Eq)

data DuDuHoXGLTextures = DuDuHoXGLTextures {
    backgroundTex :: GL.TextureObject -- ^ Texture used for background
   ,playerTex :: GL.TextureObject -- ^ Texture used for player
}

data DuDuHoXGLContext = DuDuHoXGLContext {
    dirty :: IORef Bool -- ^ Should repaint the screen?
   ,userInput :: IORef (Maybe GameInput) -- ^ User input
   ,quit :: IORef Bool -- ^ Should quit the game?
   ,world :: IORef VisibleWorld -- ^ The current state of the game
   ,textures :: IORef (Maybe DuDuHoXGLTextures) -- ^ Textures used in game
   ,player :: IORef GLPlayer
   ,state :: IORef GLState
}

mkContext :: IORef VisibleWorld -> IO DuDuHoXGLContext
mkContext world' = do
    dirty' <- newIORef True
    quit' <- newIORef False
    userInput' <- newIORef Nothing
    textures' <- newIORef Nothing
    player' <- newIORef $ GLPlayer (0, 0)
    state' <- newIORef Accept
    return DuDuHoXGLContext {
        dirty = dirty',
        userInput = userInput',
        quit = quit',
        world = world',
        textures = textures',
        player = player',
        state = state'
    }
