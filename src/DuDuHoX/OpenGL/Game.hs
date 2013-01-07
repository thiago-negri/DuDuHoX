{-# LANGUAGE RecordWildCards #-}

module DuDuHoX.OpenGL.Game where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           DuDuHoX.Game
import           DuDuHoX.OpenGL.Data
import           DuDuHoX.OpenGL.Init
import           DuDuHoX.World.Base
import           DuDuHoX.World.Types
import           DuDuHoX.World.Visible
import           Graphics.Rendering.OpenGL (($=))

game :: World -> IO ()
game w = do
    w' <- newIORef (mkVisWorld w)
    context <- mkContext keyboardCallback w'
    initGL context
    loop context
    releaseGL

loop :: DuDuHoXGLContext -> IO ()
loop (c@DuDuHoXGLContext{..}) = do
    -- redraw screen if dirty
    d <- readIORef dirty
    when d $ do
        clearScreen c
        world' <- readIORef world
        let world'' = limitViewer (WorldPosition 19 5) world'
        drawWorld world''
        when (won (vWorld world')) drawWin
        GLFW.swapBuffers
        writeIORef dirty False

    GLFW.waitEvents

    -- check for user input
    input <- tryTakeMVar userInput
    case input of
        Just Quit -> writeIORef quit True
        Just (Movement m) -> do
            world' <- readIORef world
            unless (won (vWorld world')) $
                let newWorld = updateWorld world' (movePlayer (vWorld world') m)
                in do writeIORef world newWorld
                      writeIORef dirty True
        _ -> return ()

    -- check if we need to quit the loop
    q <- readIORef quit
    unless q $ loop c

clearScreen :: DuDuHoXGLContext -> IO ()
clearScreen (DuDuHoXGLContext{..}) = do
    GL.clear [GL.ColorBuffer]
    GL.color $ color3 1 1 1
    Just tex <- readIORef backgroundTex
    with2DTexture tex $
        GL.renderPrimitive GL.TriangleStrip $ do
            GL.texCoord $ GL.TexCoord2 1 (1::GL.GLfloat)
            GL.vertex $ vertex3 800 0 0
            GL.texCoord $ GL.TexCoord2 1 (0::GL.GLfloat)
            GL.vertex $ vertex3 800 450 0
            GL.texCoord $ GL.TexCoord2 0 (1::GL.GLfloat)
            GL.vertex $ vertex3 0 0 0
            GL.texCoord $ GL.TexCoord2 0 (0::GL.GLfloat)
            GL.vertex $ vertex3 0 450 0

keyboardCallback :: MVar GameInput -> GLFW.KeyCallback
keyboardCallback userInput k e =
    when (e == GLFW.Release) $
        case k of
            GLFW.CharKey 'Q' -> putMVar userInput Quit
            GLFW.CharKey 'W' -> putMVar userInput $ Movement MoveUp
            GLFW.CharKey 'A' -> putMVar userInput $ Movement MoveLeft
            GLFW.CharKey 'S' -> putMVar userInput $ Movement MoveDown
            GLFW.CharKey 'D' -> putMVar userInput $ Movement MoveRight
            _ -> return ()

drawWin :: IO ()
drawWin =
    GL.preservingMatrix $ do
        GL.translate $ vector3 10 30 0
        GL.rotate 180 $ vector3 1 0 0
        GLFW.renderString GLFW.Fixed8x16 "You won! Press 'Q' to quit."

drawWorld :: VisibleWorld -> IO ()
drawWorld w =
    GL.preservingMatrix $ do
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
        drawPlayer (position $ viewer w)

drawPlayer :: WorldPosition -> IO ()
drawPlayer p = do
    GL.color $ color3 0.9 0.9 0.9
    drawAt p $ do
        -- head
        GL.renderPrimitive GL.Quads $ do
            GL.vertex $ vertex3 7 7 0
            GL.vertex $ vertex3 7 2 0
            GL.vertex $ vertex3 13 2 0
            GL.vertex $ vertex3 13 7 0

        -- body
        GL.renderPrimitive GL.Lines $ do
            GL.vertex $ vertex3 10 7 0
            GL.vertex $ vertex3 10 13 0

        -- arms
        GL.renderPrimitive GL.Lines $ do
            GL.vertex $ vertex3 7 9 0
            GL.vertex $ vertex3 13 9 0

        -- legs
        GL.renderPrimitive GL.Lines $ do
            GL.vertex $ vertex3 10 13 0
            GL.vertex $ vertex3 7 18 0

            GL.vertex $ vertex3 10 13 0
            GL.vertex $ vertex3 13 18 0

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
            GL.vertex $ vertex3 3 0 0
            GL.vertex $ vertex3 5 2 0

            GL.vertex $ vertex3 0 17 0
            GL.vertex $ vertex3 3 20 0

            GL.vertex $ vertex3 6 3 0
            GL.vertex $ vertex3 9 6 0

            GL.vertex $ vertex3 10 10 0
            GL.vertex $ vertex3 13 13 0

            GL.vertex $ vertex3 17 5 0
            GL.vertex $ vertex3 20 8 0

            GL.vertex $ vertex3 13 15 0
            GL.vertex $ vertex3 16 18 0

            GL.vertex $ vertex3 6 15 0
            GL.vertex $ vertex3 9 18 0

            -- /
            GL.vertex $ vertex3 9 13 0
            GL.vertex $ vertex3 6 16 0

            GL.vertex $ vertex3 5 7 0
            GL.vertex $ vertex3 2 10 0

            GL.vertex $ vertex3 18 9 0
            GL.vertex $ vertex3 15 12 0

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
            GL.vertex $ vertex3 10 8 0
            GL.vertex $ vertex3 16 17 0
            GL.vertex $ vertex3 10 13 0

            GL.vertex $ vertex3 10 8 0
            GL.vertex $ vertex3 4 17 0
            GL.vertex $ vertex3 10 13 0

            GL.vertex $ vertex3 10 8 0
            GL.vertex $ vertex3 18 8 0
            GL.vertex $ vertex3 10 12 0

            GL.vertex $ vertex3 10 8 0
            GL.vertex $ vertex3 2 8 0
            GL.vertex $ vertex3 10 12 0

            GL.vertex $ vertex3 7 10 0
            GL.vertex $ vertex3 10 2 0
            GL.vertex $ vertex3 13 10 0


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
            GL.vertex $ vertex3 18 18 0
            GL.vertex $ vertex3 20 8 0

            -- top
            GL.vertex $ vertex3 16 0 0
            GL.vertex $ vertex3 6 2 0

            -- left
            GL.vertex $ vertex3 0 4 0
            GL.vertex $ vertex3 2 14 0

            -- bottom
            GL.vertex $ vertex3 4 20 0
            GL.vertex $ vertex3 14 18 0

mkVector :: WorldPosition -> GL.Vector3 GL.GLfloat
mkVector p = GL.Vector3 x y 0
    where x = fromIntegral (worldX p) * 20
          y = fromIntegral (worldY p) * 20

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3

vector3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vector3 GL.GLfloat
vector3 = GL.Vector3

with2DTexture :: GL.TextureObject -> IO a -> IO a
with2DTexture tex a = do
    GL.textureBinding GL.Texture2D $= Just tex
    r <- a
    GL.textureBinding GL.Texture2D $= Nothing
    return r
