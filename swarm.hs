import Control.Concurrent
import Control.Monad
import Graphics.UI.SDL as SDL
import System.Exit

width = 1280
height = 720

bgColor = Pixel 0x00FFFFFF

-- Init and loop functions

main = do
    initWindow
    redraw
    
    mainLoop

initWindow = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode width height 32 [SDL.DoubleBuf]
    SDL.setCaption "Swarm" "swarm"

redraw = do
    s <- getVideoSurface
    let r = Just (Rect 0 0 width height)
    SDL.fillRect s r bgColor
    drawField
    SDL.flip s
    
eventLoop = forkIO . forever $ waitEvent >>= handleEvent

handleEvent e =  when (e == SDL.Quit) exit

exit = do
    putStrLn "done"
    SDL.quit
    exitSuccess

mainLoop = forever $ do
    redraw
    e <- pollEvent
    handleEvent e

drawField = do
    s <- getVideoSurface
    let r = Just (Rect (width `quot` 2) (height `quot` 2) 10 10)
    SDL.fillRect s r (Pixel 0x00ff0000)
