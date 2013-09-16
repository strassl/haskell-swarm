{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.List
import Graphics.UI.SDL as SDL
import System.Random
import System.Exit

width = 1280
height = 720

dotCount = 1000
dotSize = 4

stepDistance = 1
friendMultiplier = 12
enemyMultiplier = 6
averageMultiplier = 2
centerMultiplier = 1
borderMultiplier = 32
awayMultiplier = 2

privateSpace = 20 
replaceChance = 15 -- In %

white = Pixel 0x00FFFFFF
black = Pixel 0x00000000

bgColor = white

data Dot = Dot { x :: Int
               , y :: Int
               , friend :: Int
               , enemy :: Int
               } deriving (Eq,Show)

type Group = [Dot]

type MouseLoc = (Int, Int)

data Simulation = Simulation { dots :: Group
                             , mouse :: MouseLoc
                             }

type Sim = StateT Simulation IO

main = do
    initWindow

    gen <- newStdGen

    let g = populate gen dotCount
    l <- getMouseLoc
    runStateT mainLoop (Simulation g l)

initWindow = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode width height 32 [SDL.DoubleBuf]
    SDL.setCaption "Swarm" "swarm"
    

exit = do
    putStrLn "done"
    SDL.quit
    exitSuccess

-- Events
processEvents = do
    e <- liftIO pollEvent
    liftIO $ handleEvent e

    l <- liftIO getMouseLoc
    modify (\s -> s {mouse = l})

handleEvent e =  when (e == SDL.Quit) exit

getMouseLoc :: IO MouseLoc
getMouseLoc = do 
    s <- SDL.getMouseState
    return $ mouseStateToLoc s

mouseStateToLoc (x, y, _) = (x, y)

-- Ties it all together
mainLoop :: Sim ()
mainLoop = forever $ do
    redraw
    processSim
    processEvents

-- Moves the simulation
processSim :: Sim ()
processSim = do
    g <- getDots
    gen <- liftIO $ newStdGen

    x::Int <- liftIO $ getStdRandom (randomR (0,100 - replaceChance))

    let ng = if x == 0 then (step $ recalcDot gen g) else step g
    modify (\s -> s {dots = ng})

getDots :: Sim Group
getDots = do
    sim <- get
    return $ dots sim

-- Drawing
redraw :: Sim ()
redraw = do
    s <- liftIO getVideoSurface
    let r = Just (Rect 0 0 width height)
    liftIO $ SDL.fillRect s r bgColor
    drawField
    liftIO $ SDL.flip s

drawField :: Sim ()
drawField = do
    s <- liftIO getVideoSurface
    
    g <- getDots
    let recs = map dotToRec g
    let draws = map (\x -> x black) $ map (SDL.fillRect s) recs

    liftIO $ sequence_ draws

    where
        dotToRec d = Just (Rect (x d) (y d) dotSize dotSize)

-- Simulation logic
populate :: RandomGen g => g -> Int -> Group
populate g c = take c $ [Dot dx dy df de | dx <- xs | dy <- ys | df <- fs | de <- es]
    where
        (g1, g2) = split g
        (g3, g4) = split g1
        xs = randomRs (0, width) g1
        ys = randomRs (0, height) g2
        fs = randomRs (0, c-1) g3
        es = randomRs (0, c-1) g4

recalcDot :: RandomGen g => g -> Group -> Group
recalcDot g1 g = replaceAt g di nd
    where (g2, g3) = split g1
          di = randomDot g1
          d = g !! di
          f = randomDot g2
          e = randomDot g3
          randomDot gen = fst (randomR (0, dotCount -1) gen)
          nd = d { friend = f, enemy = e}

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs i x = a ++ (x:b)
    where (a, (_:b)) = splitAt i xs

step :: Group -> Group
step g = map (stepDot g) g

stepDot :: Group -> Dot -> Dot
stepDot g d = moveToCenter $ moveFromEnemy $ moveToFriend $ moveFromBorder d
    where f = g !! (friend d)
          e = g !! (enemy d)
          moveAway d = moveFrom d (findClosest g d) (stepDistance * awayMultiplier)
          moveToFriend d = moveTowards d f (stepDistance * friendMultiplier)
          moveFromEnemy d = moveFrom d e (stepDistance * enemyMultiplier)
          moveToAverage d = moveTowards d (calcAverage g) (stepDistance * averageMultiplier)
          moveToCenter d =  moveTowards d center (stepDistance * centerMultiplier)
          moveFromBorder d = if isNearBorder d then moveTowards d center (stepDistance * borderMultiplier) else d

isNearBorder :: Dot -> Bool
isNearBorder d
    | ((x d) - privateSpace) < 0 = True
    | ((x d) + privateSpace) > width = True
    | ((y d) - privateSpace) < 0 = True
    | ((y d) + privateSpace) > height = True
    | otherwise = False

calcAverage :: Group -> Dot
calcAverage g = Dot ((x aDot) `quot` dotCount) ((y aDot) `quot` dotCount) (-1) (-1)
    where aDot = foldl (\ad d -> ad {x = (x ad) + (x d), y = (y ad) + (y d)}) (Dot 0 0 (-1) (-1)) g

findClosest :: Group -> Dot -> Dot
findClosest g d = if (calcDist d closest) < privateSpace then closest else d
    where isCloser acc od =  if ((calcDist d od) < (calcDist d acc)) && d /= od then od else acc
          closest = foldl isCloser center g

calcDist :: Dot -> Dot -> Int
calcDist d1 d2 = abs (distX d1 d2) + abs (distY d1 d2)

distX :: Dot -> Dot -> Int
distX d1 d2 = (x d1) - (x d2)

distY :: Dot -> Dot -> Int
distY d1 d2 = ((y d1) - (y d2))

center :: Dot
center = Dot (width `quot` 2) (height `quot` 2) (-1) (-1)
    
moveFrom :: Dot -> Dot -> Int -> Dot
moveFrom d t dist = moveTowards d t (dist * (-1))

-- Move the dot along the axis with the greater distance
-- Trigonometric functions make little sense at this scale
moveTowards :: Dot -> Dot -> Int -> Dot
moveTowards d t dist
    | (dx == 0) && (dy == 0) = d
    | abs dx > abs dy = moveDot d (dist * (-1) * (dx `quot` (abs dx))) 0
    | otherwise = moveDot d 0 (dist  * (-1) * (dy `quot` (abs dy)))
    where dx = (x d) - (x t)
          dy = (y d) - (y t)

moveDot :: Dot -> Int -> Int -> Dot
moveDot d dx dy = d { x = (x d) + dx, y = (y d) + dy }
