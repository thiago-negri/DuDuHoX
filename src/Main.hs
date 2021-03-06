module Main where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8

import qualified DuDuHoX.Console.Game     as C
import qualified DuDuHoX.OpenGL.Loop      as OGL

import           Control.Monad
import           DuDuHoX.Console.IORunner (runConsole)
import           DuDuHoX.World.Builder    (parseWorld)
import           DuDuHoX.World.Types      (World)
import           System.Directory         (doesFileExist)
import           System.IO                (hFlush, stdout)

data UserInterface = Console | OpenGL

main :: IO ()
main = do
    userInterface <- getUserInterface
    world <- getWorld

    case userInterface of
        Console -> runConsole . C.game $ world
        OpenGL -> OGL.game world

getUserInterface :: IO UserInterface
getUserInterface = do
    putStr "User interface (console | opengl): "
    hFlush stdout
    option <- getLine
    case option of
        "console" -> return Console
        "opengl" -> return OpenGL
        _ -> do
            putStrLn "Invalid option"
            getUserInterface

getWorld :: IO World
getWorld = do
    putStr "Map name (maze | easyWorld): "
    hFlush stdout
    fileName <- liftM (("data/map/" ++) . (++ ".ddh")) getLine
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            file <- BS.readFile fileName
            let w = parseWorld . lines $ BS8.unpack file
            case w of
                Just w' -> return w'
                Nothing -> do
                    putStrLn "Invalid map"
                    getWorld
        else do
            putStrLn "Map not found"
            getWorld
