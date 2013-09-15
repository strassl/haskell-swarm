{-# LANGUAGE ParallelListComp #-}

import Control.Concurrent
import Control.Monad
import Graphics.UI.SDL as SDL
import System.Random
import System.Exit

width = 1280
height = 720

dotCount = 100
dotSize = 4

white = Pixel 0x00FFFFFF
black = Pixel 0x00000000

bgColor = white

data Dot = Dot { x :: Int
               , y :: Int
               , friend :: Int
               } deriving Show

type Group = [Dot]


main = do
    initWindow
    redraw
    
    mainLoop

initWindow = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode width height 32 [SDL.DoubleBuf]
    SDL.setCaption "Swarm" "swarm"

exit = do
    putStrLn "done"
    SDL.quit
    exitSuccess

-- Events
eventLoop = forkIO . forever $ waitEvent >>= handleEvent

handleEvent e =  when (e == SDL.Quit) exit


-- Drawing
mainLoop = forever $ do
    redraw
    e <- pollEvent
    handleEvent e

redraw = do
    s <- getVideoSurface
    let r = Just (Rect 0 0 width height)
    SDL.fillRect s r bgColor
    drawField
    SDL.flip s

drawField = do
    s <- getVideoSurface
    
    let gen = mkStdGen 42
    let g = populate gen dotCount
    let recs = map dotToRec g
    let draws = map (\x -> x black) $ map (SDL.fillRect s) recs

    sequence draws

    where
        dotToRec d = Just (Rect (x d) (y d) dotSize dotSize)



-- Simulation logic
populate :: RandomGen g => g -> Int -> Group
populate g c = take c $ [Dot dx dy df | dx <- xs | dy <- ys | df <- fs]
    where
        (g1, g2) = split g
        xs = randomRs (0, width) g1
        ys = randomRs (0, height) g2
        fs = randomRs (0, c) g1 -- You could use a distinct RNG for this, but it doesn't really matter

step :: Group -> Group
step g = map (stepDot g) g

stepDot  :: Group -> Dot -> Dot
stepDot g d = d
