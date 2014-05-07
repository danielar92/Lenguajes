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


-- dibuja :: Pixels -> IO ()
-- dibuja pixel = do
--   forM_ (dots pixel) $ \fila -> do
--     let transform True = '*'
--         transform False = ' '
--     putStrLn $ map (transform . on) fila

readEffects :: [String] -> [Effects] -> IO ([Effects])
readEffects [] accum = return accum
readEffects (ef:efs) accum = do
  let e = (read ef)::([Effects])
      newAccum = accum ++ e
  readEffects efs newAccum

readDisplayInfo :: Handle -> IO [Effects]
readDisplayInfo h = do
  contents <- hGetContents h
  return $ map read $ lines contents
  -- let conjunto = []
  --     l = lines contents
  -- readEffects l conjunto

-- | Lleva un String a su representación en Pixels
-- say :: String -> Pixels
-- say me volvi un culo messageToPixels



-- | Produce un retraso en milisegundos en representación de Pixels
delay :: Int -> IO ()
delay y = threadDelay y

-- | Repetir una serie de efectos un número finito de veces
-- repeatE :: Integer -> [Effects] ->

-- efectos :: Pixels -> IO ()
-- efectos pixel = do
--   dibuja pixel
--   threadDelay 1000000
--   efectos (left (negative pixel))
