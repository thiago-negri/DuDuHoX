{-# LANGUAGE RecordWildCards #-}

module DuDuHoX.OpenGL.Init where

import           Data.IORef

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           DuDuHoX.OpenGL.Data
import           DuDuHoX.OpenGL.Animation
import           DuDuHoX.World.Types
import           DuDuHoX.World.Visible

initGL :: World -> IO DuDuHoXGLContext
initGL w = do
    _ <- GLFW.initialize

    -- Open window
    GLFW.openWindowHint GLFW.FSAASamples 6 -- Anti-aliasing
    _ <- GLFW.openWindow (GL.Size 800 600) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowTitle $= "DuDuHoX"
    GL.shadeModel    $= GL.Smooth
    
    -- Load textures
    textures <- loadTextures >>= newIORef
    player <- loadPlayer >>= newIORef
    
    -- Build visible world
    w' <- newIORef $ mkVisWorld w
    
    -- Make context
    c <- mkContext w' player textures
            
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

    -- When the window needs a refresh, set the context dirty
    GLFW.windowRefreshCallback $= writeIORef (dirty c) True

    -- Terminate the program if the window is closed
    GLFW.windowCloseCallback $= (writeIORef (quit c) True >> return True)
    
    return c

loadTextures :: IO DuDuHoXGLTextures
loadTextures = do
    (bTexName:_) <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just bTexName
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
    _ <- GLFW.loadTexture2D "data/texture/background.tga" [GLFW.BuildMipMaps]

    return DuDuHoXGLTextures {
        backgroundTex = bTexName
    }

loadPlayer :: IO GLPlayer
loadPlayer = do
    (pTex1Name:pTex2Name:_) <- GL.genObjectNames 2
    
    GL.textureBinding GL.Texture2D $= Just pTex1Name
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
    _ <- GLFW.loadTexture2D "data/texture/player/player1.tga" [GLFW.BuildMipMaps]
    
    GL.textureBinding GL.Texture2D $= Just pTex2Name
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
    _ <- GLFW.loadTexture2D "data/texture/player/player2.tga" [GLFW.BuildMipMaps]
    
    let sprite1 = (pTex1Name, (20, 20))
        sprite2 = (pTex2Name, (20, 20))
        animation = createAnimation [(sprite1, 0.3), (sprite2, 0.3)]
    
    return $ GLPlayer (0, 0) animation
    

releaseGL :: IO ()
releaseGL = do
    GLFW.closeWindow
    GLFW.terminate
