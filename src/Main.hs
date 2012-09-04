module Main where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import           DuDuHoX.Console (game)
import           DuDuHoX.Console.Monad (runConsole)
import           DuDuHoX.World (parseWorld)
import           System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStr "Map name (maze.ddh | easyWorld.ddh): "
    hFlush stdout
    file <- getLine >>= BS.readFile
    maybe (putStrLn "Invalid map") (runConsole . game) $ parseWorld . lines $ BS8.unpack file
