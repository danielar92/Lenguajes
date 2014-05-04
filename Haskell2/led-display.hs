{-|
Module      : led-display
Description : Módulo manejador de I/O del proyecto Pixels.
Copyright   : Daniela Rodríguez, 2014
              Patrick Rengifo, 2014
-}

import qualified System.Environment as SE
import qualified Graphics.HGL as G
import qualified Effects as E
import qualified Pixels as P

ledDisplay :: Map Char Pixels -> [Effects] -> IO ()
ledDisplay d e = do
  G.runGraphics $ do
    w <- G.openWindow "Drawing Pixels" (640,480)
    G.clearWindow w
    
drawTree t = do
  G.runGraphics $ do
    w <- G.openWindow "Drawing Trees" (640,480)
    G.clearWindow w
    let l = layout t
    G.drawInWindow w $ G.overGraphics $ nodes l
    G.drawInWindow w $ G.overGraphics $ branches l
    G.getKey w
    G.closeWindow w
    
processFiles []       = putStrLn "Hasta Luego!"
processFiles (fn:fns) = do 
  handle <- openFile fn ReadMode
  makeup <- readDisplayInfo handle
--   do something
--   let t = (read s)::(Tree String)
--   drawTree t
  processFiles fns

main = do
  files <- SE.getArgs
  fontFile = head files
  handle <- openFile fontFile ReadMode
  dict <- readFont handle
  processFiles tail $ files
