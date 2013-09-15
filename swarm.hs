import Control.Concurrent
import Control.Monad
import Graphics.UI.SDL as SDL
import System.Exit

width = 1280
height = 720

white = Pixel 0x00FFFFFF
black = Pixel 0x00000000

bgColor = white

data Dot = Dot { x :: Int
               , y :: Int
               , friend :: Int } -- Index of the friend

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
    let g = [Dot 10 10 0, Dot 11 120 0, Dot 764 124 0, Dot 1200 20 0]
    let recs = map dotToRec g
    let draws = map (\x -> x black) $ map (SDL.fillRect s) recs

    sequence draws

    where
        dotToRec d = Just (Rect (x d) (y d) 2 2)



-- Simulation logic
step :: Group -> Group
step g = map (next g) g

next :: Group -> Dot -> Dot
next g d = d
