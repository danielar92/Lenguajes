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
import Control.Monad
import Control.Concurrent
import System.IO
import qualified Data.Map as M

-- | Busca el string más largo para definir tamaño de la ventana
findDimensions :: M.Map Char P.Pixels -> [E.Effects] -> (Int, Int)
findDimensions m effects = maxDimensions effects
  where maxDimensions xs = foldl go (0, 0) $ map f xs
        go (xx, y) (x', y') = if y' > y then (x', y') else (xx, y)
        f (E.Say s) = (20+5*n, 20+6*(m'*(length s)))
        f (E.Repeat _ xs) = maxDimensions xs
        f (E.Forever xs) = maxDimensions xs
        f _ = (0, 0)
        x = snd (M.elemAt 0 m)
        n = length (P.dots x)
        m' = length (head (P.dots x))

-- | Magic
isFinite :: E.Effects -> Bool
isFinite (E.Forever _ ) = False
isFinite (E.Repeat x xs) = and $ map isFinite xs
isFinite _ = True

isFiniteList :: [E.Effects] -> Bool
isFiniteList = and . map isFinite

-- | Abre una ventana con la ejecucion de las instrucciones en Efectos.
ledDisplay :: M.Map Char P.Pixels -> [E.Effects] -> IO ()
ledDisplay m e = do
  G.runGraphics $ do
    let (y, x) = findDimensions m e
    w <- G.openWindowEx "Drawing Pixels" Nothing (x, y) G.DoubleBuffered (Just 1)
    G.clearWindow w

    let cierrame  = do
          key <- G.getKey w
          putStrLn $ show key
          if G.isEscapeKey key then
            G.closeWindow w
          else
            cierrame

    let magic = do
          foldM_ (E.doEffect m w) (P.Pixels G.White []) e
          G.getKey w
          G.closeWindow w


    -- forkIO $ cierrame
    G.par_ magic cierrame

-- | Procesa los archivos de efectos
processFiles :: [String] -> IO [E.Effects]
processFiles []       = return []
processFiles (fn:fns) = do
  handle <- openFile fn ReadMode
  makeup <- E.readDisplayInfo handle
  rest <- processFiles fns
  return (makeup ++ rest)

main = do
  files <- SE.getArgs
  if (length . tail $ files) < 1 then error "al menos un archivo de efectos debe ser especificado"
                      else print "Bienvenido"
  let fontFile = head files
  handle <- openFile fontFile ReadMode
  dict <- P.readFont handle
  effects <- processFiles $ tail files
  ledDisplay dict effects
  putStrLn "Hi."
