module Main where

-- FIXME: Entrada altera sempre o primeiro jogador, nao suporta mais do que um.

import System.Console.ANSI
import System.IO
import System.Random

data Coord = 
        Coord {
                cX :: Int, -- Horizontal
                cY :: Int  -- Vertical
                }
        deriving (Eq)

data Player =
        Player {
                pName :: String, -- Nome do jogador
                pHealth :: Int, -- Vida do jogador
                pArmor :: Armor, -- Armadura usada pelo jogador
                pWeapon :: Weapon -- Arma usada pelo jogador
                }

data PotionEffect = Heal | Harm

data Potion = 
        Potion {
                ptName :: String, -- Nome da poção
                ptEffect :: PotionEffect, -- Efeito causado
                ptAmount :: Int -- Quantidade do efeito causado
                }

data Armor = 
        Armor {
                aName :: String, -- Nome da armadura
                aClass :: Int -- Quantidade de dano absorvido
                }

data Weapon =
        Weapon {
                wName :: String, -- Nome da arma
                wDamage :: Int -- Quantidade de dano causado
                }

data Obstacle =
        Obstacle {
                oName :: String, -- Nome do obstáculo
                oHealth :: Int -- Vida do obstáculo
                }

data WorldObject a =
        WorldObject {
                woObject :: a, -- Objeto no mundo
                woCoord :: Coord -- Posição do objeto
                } 

data World =
        World {
                wdPlayers :: [WorldObject Player], -- Jogadores
                wdPotions :: [WorldObject Potion], -- Poções
                wdArmors :: [WorldObject Armor], -- Armaduras
                wdWeapons :: [WorldObject Weapon], -- Armas
                wdObstacles :: [WorldObject Obstacle], -- Obstaculos
                wdPlayerSpawnAreas :: [Coord] -- Localizações que jogadores podem nascer
                }

cloth :: Armor
cloth = Armor { aName = "Roupa", aClass = 0 }

fists :: Weapon
fists = Weapon { wName = "Punhos", wDamage = 1 }

apple :: Potion
apple = Potion { ptName = "Maca", ptEffect = Heal, ptAmount = 5 }

poison :: Potion
poison = Potion { ptName = "Veneno", ptEffect = Harm, ptAmount = 5 }

wall :: Obstacle
wall = Obstacle { oName = "Parede", oHealth = 10000 } 

newPlayer :: String -> Player
newPlayer name = Player { 
        pName = name,
        pHealth = 100, 
        pArmor = cloth, 
        pWeapon = fists 
        }

emptyWorld :: World
emptyWorld = World { 
        wdPlayers = [], 
        wdPotions = [],
        wdArmors = [],
        wdWeapons = [],
        wdObstacles = [],
        wdPlayerSpawnAreas = []
        }

class InterfaceObject a where
        draw :: a -> Char

instance InterfaceObject Player where
        draw _ = '@'

instance InterfaceObject Obstacle where
        draw _ = '#'
        
instance InterfaceObject Potion where
        draw _ = '!'

instance InterfaceObject Armor where
        draw _ = '['

instance InterfaceObject Weapon where
        draw _ = '/'

myWorld :: [String]
myWorld = [
        "######",
        "# @  #",
        "#### #############################",
        "#        !              [   # @  #",
        "#   ##################      #### ##",
        "#   #             !     #   #     #",
        "#      #### # ###########   # ### #",
        "#   /  # @  #                     #",
        "###################################"
        ]

strToWorld :: [String] -> World
strToWorld = go 0 emptyWorld
        where   go _ w [] = w
                go y w (e:es) = let w' = go' 0 w e in go (y + 1) w' es
                                        where   go' _ w [] = w
                                                go' x w (e:es) = let w' = parseWorldObject e (Coord x y) w in go' (x + 1) w' es

parseWorldObject :: Char -> Coord -> World -> World
parseWorldObject '#' c w = w { wdObstacles = WorldObject wall c : wdObstacles w }
parseWorldObject '@' c w = w { wdPlayerSpawnAreas = c : wdPlayerSpawnAreas w }
parseWorldObject _ _ w = w

drawWorld :: World -> IO ()
drawWorld w = do
        clearScreen
        drawList obstacles
        drawList players
        where   drawList :: (InterfaceObject a) => [WorldObject a] -> IO () 
                drawList = mapM_ drawWorldObject
                obstacles = wdObstacles w
                players = wdPlayers w

drawWorldObject :: (InterfaceObject a) => WorldObject a -> IO ()
drawWorldObject wo = do
         setCursorPosition y x
         putChar o
         where  Coord x y = woCoord wo
                o = draw $ woObject wo

addPlayerAtRandomCoord :: String -> World -> StdGen -> (StdGen, World)
addPlayerAtRandomCoord name w g = (g', w')
        where ps = wdPlayerSpawnAreas w
              (i, g') = randomR (1, length ps) g
              w' = w { wdPlayers = wop : wdPlayers w }
              wop = WorldObject p c
              c = ps !! (i - 1)
              p = newPlayer name

main :: IO ()
main = do
        stdGen <- newStdGen
        name <- getPlayerName
        initConsole
        let (stdGen', world) = addPlayerAtRandomCoord name worldA stdGen
        setStdGen stdGen'
        gameLoop world
        freeConsole
        where worldA = strToWorld myWorld
               
gameLoop :: World -> IO ()
gameLoop world = do 
        drawWorld world
        input <- getInput
        let world' = maybe world (handleInput world) input 
        gameLoop world'

data GameInput = MoveUp | MoveDown | MoveLeft | MoveRight

getInput :: IO (Maybe GameInput)
getInput = do
        c <- getChar
        return $ charToGameInput c
        
charToGameInput :: Char -> Maybe GameInput
charToGameInput 'w' = Just MoveUp
charToGameInput 's' = Just MoveDown
charToGameInput 'a' = Just MoveLeft
charToGameInput 'd' = Just MoveRight
charToGameInput _ = Nothing

handleInput :: World -> GameInput -> World
handleInput w i = case i of
        MoveUp -> move w $ Coord 0 (-1)
        MoveDown -> move w $ Coord 0 1
        MoveLeft -> move w $ Coord (-1) 0
        MoveRight -> move w $ Coord 1 0  
        where p = head $ wdPlayers w
              c = woCoord p
              move w c' = w { wdPlayers = [p { woCoord = filterCoord w c (c |+| c') }] }

filterCoord :: World -> Coord -> Coord -> Coord
filterCoord w c c' = if null o then c' else c
        where o = filter (\wo -> woCoord wo == c') $ wdObstacles w

(|+|) :: Coord -> Coord -> Coord
Coord x y |+| Coord x' y' = Coord (x + x') (y + y')

getPlayerName :: IO String
getPlayerName = do
        putStrLn "Informe seu nome: "
        name <- getLine
        if null name then getPlayerName
                     else return name

initConsole :: IO ()
initConsole = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        hideCursor
        setTitle "DuDuHoX"
        clearScreen

freeConsole :: IO ()
freeConsole = do
        clearScreen
        showCursor
