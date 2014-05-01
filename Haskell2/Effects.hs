module Effects
       (
         Effects(..)
       ) where

import System.IO
import Control.Monad
import Pixels
import Control.Concurrent (threadDelay)

data Effects = Say String
             | Up
             | Down
             | Left
             | Right
             | Backwards
             | UpsideDown
             | Negative
             | Delay Integer
             -- | Color Color
             | Repeat Integer [Effects]
             | Forever [Effects]
             deriving (Read, Show)


dibuja :: Pixels -> IO ()
dibuja pixel = do
  forM_ (dots pixel) $ \fila -> do
    let transform True = '*'
        transform False = ' '
    putStrLn $ map (transform . on) fila


readDisplayInfo :: Handle -> IO [Effects]
readDisplayInfo h = do
  contents <- hGetContents h
  return (read contents)

-- | Desplaza una fila del Pixel hacia arriba.
-- Esto se realiza al concatenar la 'cabeza' del String al final.
up :: Pixels -> Pixels
up pixel = Pixels { dots = reverse (x : (reverse xs)) }
  where x:xs = dots pixel

-- | Desplaza una fila del Pixel hacia abajo
-- Esto se realiza usando el último elemento de los pixels,
-- y le adjuntamos el resto de la lista.
down :: Pixels -> Pixels
down pixel = Pixels { dots = last x:(init x) }
  where x = dots pixel

-- | Desplaza una columna del Pixel hacia la izquiera.
-- Mapeamos al pixels con la función move, que coloca la 'cabeza' al
-- final del string.
left :: Pixels -> Pixels
left pixel = Pixels { dots = map move x }
    where
      x = dots pixel
      move (x:xs) = reverse (x:(reverse xs))

-- | Desplaza una columan del Pixel hacia la derecha.
-- Igual que con left, mapeamos colocando el último elemento al principio
right :: Pixels -> Pixels
right pixel = Pixels { dots = (map (\x -> last x:(init x)) $ dots pixel) }

-- | Invierte el orden de las filas del Pixel.
-- Colocamos al revés el pixels con reverse.
upsideDown :: Pixels -> Pixels
upsideDown pixel = Pixels { dots = reverse (dots pixel) }

-- | Invierte el orden de las columnas del Pixel.
-- Mapeamos cada elemento con reverse.
backwards :: Pixels -> Pixels
backwards pixel = Pixels { dots = map reverse (dots pixel)}

-- | Intercambia los caracteres ' '  y '*' en el Pixel.
-- Mapeamos el mapeo con la función x, que invierte asteriscos por espacios
negative :: Pixels -> Pixels
negative pixel = Pixels { dots = map (map x) (dots pixel)}
  where x y = Pixel { on = not (on y)}



efectos :: Pixels -> IO ()
efectos pixel = do
  dibuja pixel
  threadDelay 1000000
  efectos (left (negative pixel))


-- | Abrimos el archivo font y le damos play a todo.
main = do
    putStrLn "Introduce el nombre del archivo:  "
    file <- getLine
    handle <- openFile file ReadMode
    result <- readFont handle
    let whatever = map (font result) ['a', 'b']
    mapM_ dibuja whatever
    -- putStrLn "Concat Mofos"
    -- dibuja $ pixelListToPixels []
    -- dibuja $ concatPixels whatever
    dibuja $ messageToPixels result "abcs"
    -- putStrLn "Letrica aqui:"
    -- algo <- getChar
    -- let myletter = font result algo
    -- print myletter
   -- putStrLn $ show result
    putStrLn "Done mofo."
    -- forever $ do
    efectos (messageToPixels result "abc")
