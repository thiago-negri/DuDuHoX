{-# LANGUAGE RecordWildCards #-}

module DuDuHoX.OpenGL.Loop where

import           Control.Monad
import           Data.IORef

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           DuDuHoX.Game
import           DuDuHoX.OpenGL.Data
import           DuDuHoX.OpenGL.Init
import           DuDuHoX.OpenGL.Render
import           DuDuHoX.World.Base
import           DuDuHoX.World.Types
import           DuDuHoX.World.Visible
import           Sound.ALUT

game :: World -> IO ()
game w = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> alutGame w
    
alutGame :: World -> IO ()
alutGame w = do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    buffer1 <- createBuffer $ File "data/sound/mainloop.wav"
    [source] <- genObjectNames 1
    loopingMode source $= Looping
    queueBuffers source [buffer1]
    play [source]
    context <- initGL w
    loop context
    releaseGL
    _ <- closeDevice device
    return ()

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
        fpsCounter' <- readIORef fpsCounter
        drawFPS (currentFPS fpsCounter')
        GLFW.swapBuffers
        writeIORef dirty False

    advanceFrame c

    case state' of
        Accept -> acceptInput c
        _ -> return ()

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

    up <- GLFW.getKey GLFW.UP
    down <- GLFW.getKey GLFW.DOWN
    left <- GLFW.getKey GLFW.LEFT
    right <- GLFW.getKey GLFW.RIGHT
    esc <- GLFW.getKey GLFW.ESC

    when (w == GLFW.Press || up == GLFW.Press)    $ writeIORef userInput . Just $ Movement MoveUp
    when (a == GLFW.Press || left == GLFW.Press)  $ writeIORef userInput . Just $ Movement MoveLeft
    when (s == GLFW.Press || down == GLFW.Press)  $ writeIORef userInput . Just $ Movement MoveDown
    when (d == GLFW.Press || right == GLFW.Press) $ writeIORef userInput . Just $ Movement MoveRight
    when (q == GLFW.Press || esc == GLFW.Press)   $ writeIORef userInput . Just $ Quit

    -- check for user input
    input <- readIORef userInput
    case input of
        Just Quit -> writeIORef quit True
        Just (Movement m) -> do
            world' <- readIORef world
            unless (won (vWorld world')) $
                let newWorld = updateWorld world' (movePlayer (vWorld world') m)
                in do writeIORef world newWorld
                      pl <- readIORef player
                      writeIORef player $ pl { delta = mkDelta world' newWorld }
                      writeIORef state Render
                      GLFW.time GL.$= 0
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

