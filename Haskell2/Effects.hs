{-|
Module      : Effects
Description : Módulo de implementación de los efectos de los pixeles
Copyright   : Daniela Rodríguez, 2014
              Patrick Rengifo, 2014
-}

module Effects
       (
         Effects(..),
         readDisplayInfo,
         drawDot,
         drawPixels,
         doEffect
       ) where

import System.IO
import Control.Monad
import Pixels
import qualified Data.Map as M
import Control.Concurrent (threadDelay)
import qualified Graphics.HGL as G
import Text.Read (readMaybe)

data Effects = Say String
             | Up
             | Down
             | Left
             | Right
             | Backwards
             | UpsideDown
             | Negative
             | Delay Integer
             | Color G.Color
             | Repeat Integer [Effects]
             | Forever [Effects]
             deriving (Read, Show)


-- | Dibuja los Pixels a partir de una coordenada.
drawPixels :: (Int, Int) -> Pixels -> G.Graphic
drawPixels (x, y) p = G.withColor (color p) $ G.overGraphics graphics
  where graphics = [ drawDot (y+dy, x+dx)  val | (row, dx) <- zip booleans [0,5..], (val, dy) <- zip row [0, 5..] ]
        d = dots p
        booleans = map (map on) d

-- | Dibuja la región 3x3 de cada Pixel
drawDot :: (Int, Int) -> Bool -> G.Graphic
drawDot  _ False    = G.emptyGraphic
drawDot (x, y) True = G.regionToGraphic $ G.rectangleRegion (x+1, y+1) (x+3, y+3)

-- | Recibe un Effect y procede a llamar las funciones correspondientes para
-- dibujarlo en la ventana gráfica
doEffect :: M.Map Char Pixels -> G.Window -> Pixels -> Effects -> IO (Pixels)
doEffect m w p e =
  case e of
    Say s -> do
      let newMsgPixels = messageToPixels m s
          newP = p {dots = dots newMsgPixels}
      G.clearWindow w
      G.drawInWindow w $ drawPixels (10, 10) newP
      return newP
    Up -> do
      let newP = (up p)
      G.clearWindow w
      G.drawInWindow w $ drawPixels (10, 10) newP
      return newP
    Down -> do
      let newP = (down p)
      G.clearWindow w
      G.drawInWindow w $ drawPixels (10, 10) newP
      return newP
    Effects.Left -> do
      let newP = (left p)
      G.clearWindow w
      G.drawInWindow w $ drawPixels (10, 10) newP
      return newP
    Effects.Right -> do
      let newP = (right p)
      G.clearWindow w
      G.drawInWindow w $ drawPixels (10, 10) newP
      return newP
    Backwards -> do
      let newP = (backwards p)
      G.clearWindow w
      G.drawInWindow w $ drawPixels (10, 10) newP
      return newP
    UpsideDown -> do
      let newP = (upsideDown p)
      G.clearWindow w
      G.drawInWindow w $ drawPixels (10, 10) newP
      return newP
    Negative -> do
      let newP = (negative p)
      G.clearWindow w
      G.drawInWindow w $ drawPixels (10, 10) newP
      return newP
    Delay t -> do
      threadDelay ((fromIntegral t) * 1000)
      putStrLn "DELAY"
      return p
    Color c -> do
      putStrLn $ show c
      let newP = p { color = c }
      putStrLn $ show (color newP)
      G.clearWindow w
      G.drawInWindow w $ drawPixels (10, 10) newP
      return newP
    Repeat 0 xs ->
      return p
    Repeat t xs -> do
      newPixel <- foldM (doEffect m w) p xs
      putStrLn "REPEAT"
      doEffect m w newPixel (Repeat (t-1) xs)
    Forever xs -> do
      newPixel <- foldM (doEffect m w) p xs
      putStrLn "FOREVER"
      doEffect m w newPixel (Forever xs)
      

-- | Obtiene el contenido del archivo de Efectos a aplicar.
readDisplayInfo :: Handle -> IO [Effects]
readDisplayInfo h = do
  contents <- hGetContents h
  return $ map read $ lines contents