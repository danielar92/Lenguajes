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
import System.IO
import Data.Map

-- | Dunno
ledDisplay :: Map Char P.Pixels -> [E.Effects] -> IO ()
ledDisplay d e = do
  G.runGraphics $ do
    w <- G.openWindow "Drawing Pixels" (640,480)
    G.clearWindow w
--     G.drawInWindow w $ G.overGraphics $ nodes l --The shit to draw pixels
    key <- G.getKey w --Aqui se pondra tecla <- G.getKey para saber si tecla = Esc y terminar la vaina esta.
--     if key == '\ESC' then G.closeWindow w else --algo
    G.closeWindow w

-- | Procesa los archivos de efectos para crear una lista unificada de todos juntos.
processFiles :: [E.Effects] -> [String] -> IO ([E.Effects])
processFiles x []       = return $ x
processFiles accum (fn:fns) = do
  handle <- openFile fn ReadMode
  makeup <- E.readDisplayInfo handle
  let newAccum = accum ++ makeup
  processFiles newAccum fns

main = do
  files <- SE.getArgs
  let fontFile = head files
  handle <- openFile fontFile ReadMode
  dict <- P.readFont handle
  eff <- processFiles [] $ tail files
  print eff
  return ()
  
  -- calculo de tamano
  --ledDisplay dict
  -- putStrLn "Hi."
