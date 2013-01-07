module DuDuHoX.OpenGL.Data where

import           Control.Concurrent
import           Data.IORef

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           DuDuHoX.Game
import           DuDuHoX.World.Types

data DuDuHoXGLContext = DuDuHoXGLContext {
    dirty :: IORef Bool -- ^ Should repaint the screen?
   ,userInput :: MVar GameInput -- ^ User input
   ,quit :: IORef Bool -- ^ Should quit the game?
   ,keyCallback :: GLFW.KeyCallback -- ^ Callback used when a key is pressed
   ,world :: IORef VisibleWorld -- ^ The current state of the game
   ,backgroundTex :: IORef (Maybe GL.TextureObject) -- ^ Texture used for background
}

mkContext :: (MVar GameInput -> GLFW.KeyCallback) -> IORef VisibleWorld -> IO DuDuHoXGLContext
mkContext keyCallback' world' = do
    dirty' <- newIORef True
    quit' <- newIORef False
    userInput' <- newEmptyMVar
    backgroundTex' <- newIORef Nothing
    return DuDuHoXGLContext {
        dirty = dirty',
        userInput = userInput',
        quit = quit',
        keyCallback = keyCallback' userInput',
        world = world',
        backgroundTex = backgroundTex'
    }
