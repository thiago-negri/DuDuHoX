{-# LANGUAGE RecordWildCards #-}

module DuDuHoX.OpenGL.Init where

import           Data.IORef

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           DuDuHoX.OpenGL.Data

initGL :: DuDuHoXGLContext -> IO ()
initGL (DuDuHoXGLContext{..}) = do
    _ <- GLFW.initialize

    -- Open window
    _ <- GLFW.openWindow (GL.Size 800 300) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowTitle $= "DuDuHoX"
    GL.shadeModel    $= GL.Smooth

    -- Enable antialiasing
    GL.lineSmooth $= GL.Enabled
    GL.polygonSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.lineWidth  $= 1.5

    -- Set the color to clear background
    GL.clearColor $= GL.Color4 0 0 0 0

    -- Set 2D orthogonal view inside windowSizeCallback because
    -- any change to the Window size should result in different
    -- OpenGL Viewport.
    GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
        GL.viewport   $= (GL.Position 0 0, size)
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

    -- Disable auto polling of events in swapBuffers
    GLFW.disableSpecial GLFW.AutoPollEvent

    -- When the window needs a refresh, set the context dirty
    GLFW.windowRefreshCallback $= writeIORef dirty True

    -- Terminate the program if the window is closed
    GLFW.windowCloseCallback $= (writeIORef quit True >> return True)

    -- Bind callback for keyboard
    GLFW.keyCallback $= keyCallback

releaseGL :: IO ()
releaseGL = do
  GLFW.closeWindow
  GLFW.terminate
