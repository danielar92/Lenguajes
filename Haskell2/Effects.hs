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

-- | Desplaza una fila del Pixel hacia arriba.
up :: Pixels -> Pixels
up pixel = Pixels {color = G.White, dots = reverse (x : (reverse xs)) }
  where x:xs = dots pixel

-- | Desplaza una fila del Pixel hacia abajo
down :: Pixels -> Pixels
down pixel = Pixels {color = G.White, dots = last x:(init x) }
  where x = dots pixel

-- | Desplaza una columna del Pixel hacia la izquierda.
left :: Pixels -> Pixels
left pixel = Pixels { color = G.White,dots = map move x }
    where
      x = dots pixel
      move (x:xs) = reverse (x:(reverse xs))

-- | Desplaza una columan del Pixel hacia la derecha.
right :: Pixels -> Pixels
right pixel = Pixels {color = G.White, dots = (map (\x -> last x:(init x)) $ dots pixel) }

-- | Invierte el orden de las filas del Pixel.
upsideDown :: Pixels -> Pixels
upsideDown pixel = Pixels {color = G.White, dots = reverse (dots pixel) }

-- | Invierte el orden de las columnas del Pixel.
backwards :: Pixels -> Pixels
backwards pixel = Pixels {color = G.White, dots = map reverse (dots pixel)}

-- | Intercambia los caracteres ' '  y '*' en el Pixel.
negative :: Pixels -> Pixels
negative pixel = Pixels { color = G.White,dots = map (map x) (dots pixel)}
  where x y = Pixel { on = not (on y)}

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


-- | Abrimos el archivo font y le damos play a todo.
-- main = do
--     putStrLn "Introduce el nombre del archivo:  "
--     file <- getLine
--     handle <- openFile file ReadMode
--     result <- readFont handle
--     let whatever = map (font result) ['a', 'b']
--     mapM_ dibuja whatever
    -- putStrLn "Concat Mofos"
    -- dibuja $ pixelListToPixels []
    -- dibuja $ concatPixels whatever
--     dibuja $ messageToPixels result "abcs"
    -- putStrLn "Letrica aqui:"
    -- algo <- getChar
    -- let myletter = font result algo
    -- print myletter
   -- putStrLn $ show result
--     putStrLn "Done mofo."
    -- forever $ do
--     efectos (messageToPixels result "abc")
