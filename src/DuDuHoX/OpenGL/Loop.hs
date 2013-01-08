{-# LANGUAGE RecordWildCards #-}

module DuDuHoX.OpenGL.Loop where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef

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
        drawWorld c world''
        when (won (vWorld world')) drawWin
        GLFW.swapBuffers
        writeIORef dirty False

    -- wait for events
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
