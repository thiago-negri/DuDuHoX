module DuDuHoX.OpenGL.Data where

import           Control.Concurrent
import           Data.IORef

import qualified Graphics.UI.GLFW   as GLFW

import           DuDuHoX.Game
import           DuDuHoX.World

data DuDuHoXGLContext = DuDuHoXGLContext {
    dirty :: IORef Bool -- ^ Should repaint the screen?
   ,userInput :: MVar GameInput -- ^ User input
   ,quit :: IORef Bool -- ^ Should quit the game?
   ,keyCallback :: GLFW.KeyCallback -- ^ Callback used when a key is pressed
   ,world :: IORef World -- ^ The current state of the game
}

mkContext :: (MVar GameInput -> GLFW.KeyCallback) -> IORef World -> IO DuDuHoXGLContext
mkContext keyCallback' world' = do
    dirty' <- newIORef True
    quit' <- newIORef False
    userInput' <- newEmptyMVar
    return DuDuHoXGLContext {
        dirty = dirty',
        userInput = userInput',
        quit = quit',
        keyCallback = keyCallback' userInput',
        world = world'
    }
