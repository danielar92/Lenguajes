{-|
Module      : Effects
Description : Módulo de implementación de los efectos de los pixeles
Copyright   : Daniela Rodríguez, 2014
              Patrick Rengifo, 2014
-}

module Effects
       (
         Effects(..),
         readDisplayInfo
       ) where

import System.IO
import Control.Monad
import Pixels
import Control.Concurrent (threadDelay)
import qualified Graphics.HGL as G

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

readDisplayInfo :: Handle -> IO [Effects]
readDisplayInfo h = do
  contents <- hGetContents h
  return $ map read $ lines contents

-- | Produce un retraso en milisegundos en representación de Pixels
delay :: Int -> IO ()
delay y = threadDelay y

-- | Dado el arreglo con todos los efectos busca el String más largo a dibujar
findLargest :: [Effects] -> Int
findLargest ef = foldr lookForMe 0 ef
  where lookForMe (Say l) x = if (length l) > x then length l else x
        lookForMe _ x       = x

-- | Lleva un String a su representación en Pixels
-- say :: String -> Pixels
-- say me volvi un culo messageToPixels

-- | Repetir una serie de efectos un número finito de veces
-- repeatE :: Integer -> [Effects] ->

-- efectos :: Pixels -> IO ()
-- efectos pixel = do
--   dibuja pixel
--   threadDelay 1000000
--   efectos (left (negative pixel))
