{-
--Module      : led-display
--Description : Módulo manejador de I/O del proyecto Pixels.
--Copyright   : Daniela Rodríguez, 2014
--              Patrick Rengifo, 2014
-}
--

import qualified System.Environment as SE
import qualified Graphics.HGL as G
import qualified Effects as E
import qualified Pixels as P
import Control.Monad
import Control.Concurrent
import System.IO
import qualified Data.Map as M
import Data.Maybe
import Data.Char

-- | Dibuja un pixel en la ventana.
perform :: G.Window -> P.Pixels -> IO (P.Pixels)
perform w pixel = do
  G.clearWindow w
  G.drawInWindow w $ P.drawPixels (10, 10) pixel
  return pixel

-- | Dado un Effects, buscamos la acción correspondiente, lo efectuamos y
-- pintamos el resultado en la ventana. Retorna el Pixels resultante.
doEffect :: M.Map Char P.Pixels -> G.Window -> P.Pixels -> E.Effects -> IO (P.Pixels)
doEffect m w p e =
  case e of
    E.Say s -> do
      let newMsgPixels = P.messageToPixels m s
          newP = p {P.dots = P.dots newMsgPixels}
      perform w newP
    E.Up ->
      perform w (P.up p)
    E.Down ->
      perform w (P.down p)
    E.Left ->
      perform w (P.left p)
    E.Right ->
      perform w (P.right p)
    E.Backwards ->
      perform w (P.backwards p)
    E.UpsideDown ->
      perform w (P.upsideDown p)
    E.Negative -> do
      perform w (P.negative p)
    E.Delay t -> do
      threadDelay ((fromIntegral t) * 1000)
      return p
    E.Color c -> do
      let newP = p { P.color = c }
      perform w newP
    E.Repeat 0 _ ->
      return p
    E.Repeat t xs -> do
      newPixel <- foldM (doEffect m w) p xs
      doEffect m w newPixel (E.Repeat (t-1) xs)
    E.Forever xs -> do
      newPixel <- foldM (doEffect m w) p xs
      doEffect m w newPixel (E.Forever xs)

-- | Abrimos y cargamos el archivo de efectos.
readDisplayInfo :: Handle -> IO [E.Effects]
readDisplayInfo h = do
  contents <- hGetContents h
  return (map E.readEffects (lines contents))


-- | Buscamos el string a imprimir más largo para calcular el tamaño de la ventana.
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




-- | Creamos la ventana en donde se pintaran los Pixels. Se mantiene escuchando
-- cuando haya una interrupción de teclado.
ledDisplay :: M.Map Char P.Pixels -> [E.Effects] -> IO ()
ledDisplay m [] = do
  putStrLn "Lista de efectos vacia. Saliendo."
  return ()
ledDisplay m e = do
  G.runGraphics $ do
    let (y, x) = findDimensions m e
    w <- G.openWindowEx "Drawing Pixels" Nothing (x, y) G.DoubleBuffered (Just 1)
    G.clearWindow w
    start <- G.getTime
    let finite = E.isFinite e
        runTime = E.calcRunning e
    let closeMe  = do
          key <- G.getKey w
          now <- G.getTime
          if (G.isEscapeKey key) || (finite && now-start >= runTime)
          then G.closeWindow w
          else closeMe

    let magic = do
          foldM_ (doEffect m w) (P.Pixels G.White []) e
          G.getKey w
          G.closeWindow w

    G.par_ magic closeMe

-- | Transformamo la lista de strings proveniente del archivo a una lista de Effects
processFiles :: [String] -> IO [E.Effects]
processFiles []       = return []
processFiles (fn:fns) = do
  handle <- openFile fn ReadMode
  makeup <- readDisplayInfo handle
  rest <- processFiles fns
  return (makeup ++ rest)

-- | Tomamos los archivos como argumentos. Abrimos y corremos el programa.
main :: IO ()
main = do
  files <- SE.getArgs
  case files of
    (fontFile:eff:effs) -> do
      handle <- openFile fontFile ReadMode
      fontsContent <- hGetContents handle
      let dict = P.parseFonts fontsContent
      effects <- processFiles $ tail files
      ledDisplay dict effects

    _ -> error "Debes proveer al menos un archivo de font y un archivo de efectos"
