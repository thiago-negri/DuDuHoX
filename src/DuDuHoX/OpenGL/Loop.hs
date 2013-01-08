{-# LANGUAGE RecordWildCards #-}

module DuDuHoX.OpenGL.Loop where

import           Control.Monad
import           Data.IORef

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           DuDuHoX.Game
import           DuDuHoX.OpenGL.Data
import           DuDuHoX.OpenGL.Init
import           DuDuHoX.OpenGL.Render
import           DuDuHoX.World.Base
import           DuDuHoX.World.Types
import           DuDuHoX.World.Visible

game :: World -> IO ()
game w = do
    w' <- newIORef (mkVisWorld w)
    context <- mkContext w'
    initGL context
    loop context
    releaseGL

loop :: DuDuHoXGLContext -> IO ()
loop (c@DuDuHoXGLContext{..}) = do
    state' <- readIORef state
    
    -- redraw screen if dirty
    d <- readIORef dirty
    when d $ do
        clearScreen c
        world' <- readIORef world
        let world'' = limitViewer (WorldPosition 19 5) world'
        drawWorld c world''
        when (won (vWorld world') && state' == Accept) drawWin
        GLFW.swapBuffers
        writeIORef dirty False

    case state' of
        Accept -> acceptInput c
        Render -> advanceFrame c

    -- check if we need to quit the loop
    q <- readIORef quit
    unless q $ loop c

acceptInput :: DuDuHoXGLContext -> IO ()
acceptInput (DuDuHoXGLContext{..}) = do
    GLFW.pollEvents
    
    writeIORef userInput Nothing
    
    q <- GLFW.getKey 'Q'
    w <- GLFW.getKey 'W'
    a <- GLFW.getKey 'A'
    s <- GLFW.getKey 'S'
    d <- GLFW.getKey 'D'
    
    when (w == GLFW.Press) $ writeIORef userInput . Just $ Movement MoveUp
    when (a == GLFW.Press) $ writeIORef userInput . Just $ Movement MoveLeft
    when (s == GLFW.Press) $ writeIORef userInput . Just $ Movement MoveDown
    when (d == GLFW.Press) $ writeIORef userInput . Just $ Movement MoveRight
    when (q == GLFW.Press) $ writeIORef userInput . Just $ Quit

    -- check for user input
    input <- readIORef userInput
    case input of
        Just Quit -> writeIORef quit True
        Just (Movement m) -> do
            world' <- readIORef world
            unless (won (vWorld world')) $
                let newWorld = updateWorld world' (movePlayer (vWorld world') m)
                in do writeIORef world newWorld
                      writeIORef player . GLPlayer $ mkDelta world' newWorld
                      writeIORef state Render
                      GLFW.time $= 0
                      writeIORef dirty True
        _ -> return ()

mkDelta :: VisibleWorld -> VisibleWorld -> (GL.GLfloat, GL.GLfloat)
mkDelta old new = d
    where
        oldPP = position $ viewer old
        newPP = position $ viewer new
        deltaP = newPP |-| oldPP
        a = worldX deltaP
        b = worldY deltaP
        d = (fromIntegral a, fromIntegral b)

