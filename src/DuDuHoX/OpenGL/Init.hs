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
    GLFW.openWindowHint GLFW.FSAASamples 6 -- Anti-aliasing
    _ <- GLFW.openWindow (GL.Size 800 450) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowTitle $= "DuDuHoX"
    GL.shadeModel    $= GL.Smooth
    
    -- Load textures
    GL.texture GL.Texture2D $= GL.Enabled
    (texName:_) <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just texName
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
    _ <- GLFW.loadTexture2D "background.tga" [GLFW.BuildMipMaps]
    writeIORef backgroundTex $ Just texName
    
    -- Enable transparency
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

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
