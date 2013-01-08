{-# LANGUAGE RecordWildCards #-}

module DuDuHoX.OpenGL.Render where

import           Control.Monad
import           Data.IORef
import           Data.Maybe

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           DuDuHoX.OpenGL.Data
import           DuDuHoX.World.Base
import           DuDuHoX.World.Types
import           DuDuHoX.World.Visible
import           Graphics.Rendering.OpenGL (($=), get)

clearScreen :: DuDuHoXGLContext -> IO ()
clearScreen (DuDuHoXGLContext{..}) = do
    GL.clear [GL.ColorBuffer]
    GL.color $ color3 1 1 1

drawWorld :: DuDuHoXGLContext -> VisibleWorld -> IO ()
drawWorld (c@DuDuHoXGLContext{..}) w = do
    p <- readIORef player
    GL.preservingMatrix $ do
        GL.translate $ mkNegDeltaVector (delta p)
        GL.translate $ vector3 10 50 0

        -- VISIBLE
        let (vFloors, vWalls, vExits) = filterFloorWallExit $ seen w
        mapM_ (drawVisibleFloor . position) vFloors
        mapM_ (drawVisibleWall . position) vWalls
        mapM_ (drawVisibleExit . position) vExits

        -- FOG
        let (fFloors, fWalls, fExits) = filterFloorWallExit $ fog w
        mapM_ (drawFogFloor . position) fFloors
        mapM_ (drawFogWall . position) fWalls
        mapM_ (drawFogExit . position) fExits

        -- player
        GL.translate $ mkDeltaVector (delta p)
        drawPlayer c (position $ viewer w)
        
    tex <- liftM (backgroundTex . fromJust) $ readIORef textures
    with2DTexture tex $
        GL.renderPrimitive GL.TriangleStrip $ do
            trTexCoord; vertex 800 0 0
            brTexCoord; vertex 800 450 0
            tlTexCoord; vertex 0 0 0
            blTexCoord; vertex 0 450 0

drawWin :: IO ()
drawWin =
    GL.preservingMatrix $ do
        GL.translate $ vector3 10 30 0
        GL.rotate 180 $ vector3 1 0 0
        --GLFW.renderString GLFW.Fixed8x16 "You won! Press 'Q' to quit."

drawPlayer :: DuDuHoXGLContext -> WorldPosition -> IO ()
drawPlayer (DuDuHoXGLContext{..}) p = do
    GL.color $ color3 1 1 1
    pl <- readIORef player
    let tO = texOff pl
    textures' <- liftM fromJust $ readIORef textures
    let (tex:next) = playerTex textures'
    when (tO > 1) $ do
        writeIORef textures . Just $ textures' { playerTex = next }
        writeIORef player $ pl { texOff = 0 }

    with2DTexture tex . drawAt p $
        GL.renderPrimitive GL.TriangleStrip $ do
            trTexCoord; vertex 20 0 0
            brTexCoord; vertex 20 20 0
            tlTexCoord; vertex 0 0 0
            blTexCoord; vertex 0 20 0

{-
    GL.color $ color3 0.9 0.9 0.9
    drawAt p $ do
        -- head
        GL.renderPrimitive GL.Quads $ do
            vertex 7 7 0
            vertex 7 2 0
            vertex 13 2 0
            vertex 13 7 0

        -- body
        GL.renderPrimitive GL.Lines $ do
            vertex 10 7 0
            vertex 10 13 0

        -- arms
        GL.renderPrimitive GL.Lines $ do
            vertex 7 9 0
            vertex 13 9 0

        -- legs
        GL.renderPrimitive GL.Lines $ do
            vertex 10 13 0
            vertex 7 18 0

            vertex 10 13 0
            vertex 13 18 0
-}

drawVisibleFloor :: WorldPosition -> IO ()
drawVisibleFloor p = drawFloor p True

drawFogFloor :: WorldPosition -> IO ()
drawFogFloor p = drawFloor p False

drawFloor :: WorldPosition -> Bool -> IO ()
drawFloor p vis = do
    -- base
    when vis $ GL.color $ color3 0.1 0.3 0
    unless vis $ GL.color $ color3 0.05 0.15 0
    drawQuad p

    -- grass
    when vis $ GL.color $ color3 0 0.5 0
    unless vis $ GL.color $ color3 0 0.25 0
    drawAt p $
        GL.renderPrimitive GL.Lines $ do
            -- \
            vertex 3 0 0
            vertex 5 2 0

            vertex 0 17 0
            vertex 3 20 0

            vertex 6 3 0
            vertex 9 6 0

            vertex 10 10 0
            vertex 13 13 0

            vertex 17 5 0
            vertex 20 8 0

            vertex 13 15 0
            vertex 16 18 0

            vertex 6 15 0
            vertex 9 18 0

            -- /
            vertex 9 13 0
            vertex 6 16 0

            vertex 5 7 0
            vertex 2 10 0

            vertex 18 9 0
            vertex 15 12 0

drawVisibleWall :: WorldPosition -> IO ()
drawVisibleWall p = do
    GL.color $ color3 0.6 0.4 0
    drawQuad p

drawFogWall :: WorldPosition -> IO ()
drawFogWall p = do
    GL.color $ color3 0.3 0.2 0
    drawQuad p

drawVisibleExit :: WorldPosition -> IO ()
drawVisibleExit p = do
    GL.color $ color3 1 1 0
    drawExit p

drawFogExit :: WorldPosition -> IO ()
drawFogExit p = do
    GL.color $ color3 0.5 0.5 0
    drawExit p

drawExit :: WorldPosition -> IO ()
drawExit p =
    drawAt p $
        GL.renderPrimitive GL.Triangles $ do
            vertex 10 8 0
            vertex 16 17 0
            vertex 10 13 0

            vertex 10 8 0
            vertex 4 17 0
            vertex 10 13 0

            vertex 10 8 0
            vertex 18 8 0
            vertex 10 12 0

            vertex 10 8 0
            vertex 2 8 0
            vertex 10 12 0

            vertex 7 10 0
            vertex 10 2 0
            vertex 13 10 0


drawAt :: WorldPosition -> IO () -> IO ()
drawAt p a =
    GL.preservingMatrix $ do
        GL.translate $ mkVector p
        a

drawQuad :: WorldPosition -> IO ()
drawQuad p =
    drawAt p $
        GL.renderPrimitive GL.Polygon $ do
            -- right
            vertex 18 18 0
            vertex 20 8 0

            -- top
            vertex 16 0 0
            vertex 6 2 0

            -- left
            vertex 0 4 0
            vertex 2 14 0

            -- bottom
            vertex 4 20 0
            vertex 14 18 0

mkVector :: WorldPosition -> GL.Vector3 GL.GLfloat
mkVector p = GL.Vector3 x y 0
    where x = fromIntegral (worldX p) * 20
          y = fromIntegral (worldY p) * 20

vertex :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> IO ()
vertex a b c = GL.vertex $ vertex3 a b c

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3

vector3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vector3 GL.GLfloat
vector3 = GL.Vector3

texCoord2 :: GL.GLfloat -> GL.GLfloat -> GL.TexCoord2 GL.GLfloat
texCoord2 = GL.TexCoord2

blTexCoord :: IO ()
blTexCoord = GL.texCoord $ texCoord2 0 0

brTexCoord :: IO ()
brTexCoord = GL.texCoord $ texCoord2 1 0

tlTexCoord :: IO ()
tlTexCoord = GL.texCoord $ texCoord2 0 1

trTexCoord :: IO ()
trTexCoord = GL.texCoord $ texCoord2 1 1

with2DTexture :: GL.TextureObject -> IO a -> IO a
with2DTexture tex a = do
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just tex
    r <- a
    GL.textureBinding GL.Texture2D $= Nothing
    GL.texture GL.Texture2D $= GL.Disabled
    return r

advanceFrame :: DuDuHoXGLContext -> IO ()
advanceFrame (DuDuHoXGLContext{..}) = do
        t <- get GLFW.time
        let mov = realToFrac $ 60 * t
        let texAdv = 6 * t
        GLFW.time $= 0
        
        p <- readIORef player
        let (a, b) = delta p
    
            a' | a > 0     = a + mov
               | a < 0     = a - mov
               | otherwise = 0
    
            b' | b > 0     = b + mov
               | b < 0     = b - mov
               | otherwise = 0
    
        if a >= 20 || a <= -20 || b >= 20 || b <= -20 || (a == 0 && b == 0)
            then do
                writeIORef player $ p { delta = (0, 0), texOff = (texOff p) + texAdv }
                writeIORef dirty True
                writeIORef state Accept
            else do
                writeIORef player $ p { delta = (a', b'), texOff = (texOff p) + texAdv }
                writeIORef dirty True


mkNegDeltaVector :: (GL.GLfloat, GL.GLfloat) -> GL.Vector3 GL.GLfloat
mkNegDeltaVector (a, b) = vector3 (-a') (-b') 0
    where
        a' = da + a
        b' = db + b
        
        da | a > 0     = -20
           | a < 0     = 20
           | otherwise = 0
           
        db | b > 0     = -20
           | b < 0     = 20
           | otherwise = 0

mkDeltaVector :: (GL.GLfloat, GL.GLfloat) -> GL.Vector3 GL.GLfloat
mkDeltaVector (a, b) = vector3 (da + a) (db + b) 0
    where
        da | a > 0     = -20
           | a < 0     = 20
           | otherwise = 0
           
        db | b > 0     = -20
           | b < 0     = 20
           | otherwise = 0
