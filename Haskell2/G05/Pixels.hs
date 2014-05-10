{-|
Module      : Pixels
Description : Módulo que permite pasar un texto y transformarlo en Pixeles
                de LED Displays.
Copyright   : Daniela Rodriguez, 2014
              Patrick Rengifo, 2014
-}

module Pixels
       (
         parseFonts,
         messageToPixels,
         font,
         Pixels(..),
         Pixel(..),
         up,
         down,
         left,
         right,
         backwards,
         upsideDown,
         negative,
         drawPixels
       ) where

import qualified Data.Map as M
import qualified Graphics.HGL as G

-- | Modificación del Pixels que incluye color y una lista de Pixel
-- representando los puntos.
data Pixels = Pixels { color :: G.Color
                     , dots ::[[Pixel]] }
           deriving (Show)

-- | Representación del Pixel encendido o apagado.
data Pixel = Pixel { on :: Bool } deriving Show

-- | A través de un diccionario de símbolos y Pixels, y un caracter,
-- transforma dicho caracter en el tipo Pixels
font :: M.Map Char Pixels -> Char -> Pixels
font dicc wanted = if (M.member wanted dicc)
                   then dicc M.! wanted
                   else let x = snd (M.elemAt 0 dicc)
                            n = length (dots x) -- filas
                            m = length ((dots x) !! 0) -- columnas
                        in Pixels {color = G.White,
                                   dots=[[Pixel True | y <- [1..m]] | x <- [1..n]]}

-- | Recibe una lista de varios Pixels y los concatena con un Pixel vacío entre
-- cada uno.
pixelListToPixels :: [Pixels] -> Pixels
pixelListToPixels xs = Pixels {color = G.White,  dots = newDots }
  where newDots = concatMap dots xs

-- | Recibe una lista de varios Pixels y los concatena en uno solo.
concatPixels :: [Pixels] -> Pixels
concatPixels xs = Pixels {color = G.White, dots = newDots} where
  newDots = foldr (zipWith (++)) initial $ initialDots
  initialDots = map dots xs
  initial = if null initialDots
            then []
            else [[] | x <- [1..(length (initialDots !! 0))] ]

-- | Toma un diccionario de varios caracteres Pixels y transforma en un único Pixels
messageToPixels :: M.Map Char Pixels ->  String -> Pixels
messageToPixels m xs = if length xs == 0
                       then Pixels { color = G.White, dots = [] }
                       else Pixels {color = G.White, dots = newDots} where
  newDots = foldl1 (zipWith (addWhitespace)) $ initialDots
  initialDots = map dots $ map (font m) xs
  addWhitespace x y = x ++ [whitespace] ++ y
  whitespace = Pixel { on = False }

-- | A través de un font y el número de filas del mismo, lo representamos como
-- una lista.
partition :: [a] -> Int -> [[a]]
partition [] _ = []
partition list n = take n list : partition (drop n list) n

-- | Verificación que existe sólo un símbolo entre las comillas.
readLetter :: String -> Char
readLetter ('"':letter:'"':xs) = letter
readLetter x = error $  "Formato incorrecto especificando una letra." ++ (show x)

-- | Verificación de la definición de pixeles.
readLetterRep = map (\x -> if check x
                           then x
                           else error "Caracteres inesperados")

-- | Verificación de la definición de pixeles sea sólo astericos o espacios
-- en blanco.
check = all inRange
  where inRange x = x == '*' || x == ' '

-- | Verificación del tamaño de las filas y columnas con la especificación del Pixel
checkSize :: Int -> Int -> [String] -> [String]
checkSize row col list = if (length list) /= row+1
                         then error "No furulan las filas"
                         else (head list) : map (\x -> if length x /= col
                                         then error "No furulan las columnas."
                                         else x ) (tail list)

-- | Se toma el tamaño de los pixels y una lista de ellos y se transforman en
-- en un dicccionario de Pixels para representarlo en el Monad.
readEachLetter :: Int -> Int -> [[String]] -> M.Map Char Pixels -> M.Map Char Pixels
readEachLetter _ _ [] accum = accum
readEachLetter n m letters accum =
  let (letterCod:current) = checkSize n m  (head letters)
      letter = readLetter letterCod
      newAccum = M.insert letter pixels accum
      pixels = Pixels { color = G.White,
                        dots = map transform (readLetterRep current) }
      transform x = map transformChar x
      transformChar '*' = Pixel { on = True }
      transformChar ' ' = Pixel { on = False }
  in readEachLetter n m (tail letters)  newAccum

-- | Hace un parse de un string que representa un diccionario de
-- Char -> Pixels.
parseFonts :: String -> M.Map Char Pixels
parseFonts contents = if length numbers /= 2
                      then error "No hay dos numeros en la primera linea"
                      else readEachLetter row col letters M.empty
  where numbers = if null l then error "Archivo fonts vacio." else words (head l)
        l = lines contents
        col = read (head numbers) :: Int
        row = read (head (tail numbers)) :: Int
        letters = partition (tail l) (row+1)


-- | Desplaza una fila del Pixel hacia arriba.
up :: Pixels -> Pixels
up pixel = pixel {dots = reverse (x : (reverse xs)) }
  where x:xs = dots pixel



-- | Desplaza una fila del Pixel hacia abajo
down :: Pixels -> Pixels
down pixel = pixel {dots = last x:(init x) }
  where x = dots pixel

-- | Desplaza una columna del Pixel hacia la izquierda.
left :: Pixels -> Pixels
left pixel = pixel { dots = map move x }
    where
      x = dots pixel
      move (x:xs) = reverse (x:(reverse xs))

-- | Desplaza una columan del Pixel hacia la derecha.
right :: Pixels -> Pixels
right pixel = pixel {dots = (map (\x -> last x:(init x)) $ dots pixel) }

-- | Invierte el orden de las filas del Pixel. http://www.upsidedowntext.com/
upsideDown :: Pixels -> Pixels
upsideDown pixel = let np = backwards pixel in np { dots = reverse (dots np) }
-- [[Pixel]]
-- | Invierte el orden de las columnas del Pixel.
backwards :: Pixels -> Pixels
backwards pixel = pixel {dots = map reverse (dots pixel)}

-- | Intercambia los caracteres ' '  y '*' en el Pixel.
negative :: Pixels -> Pixels
negative pixel = pixel { dots = map (map x) (dots pixel)}
  where x y = Pixel { on = not (on y)}

-- | Dibuja un Pixels partiendo de un punto dado.
drawPixels :: (Int, Int) -> Pixels -> G.Graphic
drawPixels (x, y) p = G.withColor (color p) $ G.overGraphics graphics
  where graphics = [ drawDot (y+dy, x+dx)  val |
                     (row, dx) <- zip booleans [0,5..],
                     (val, dy) <- zip row [0, 5..] ]
        d = dots p
        booleans = map (map on) d

-- | Dibuja la región 3x3 de cada Pixel con su margen de 1 pixel real.
drawDot :: (Int, Int) -> Bool -> G.Graphic
drawDot _  False = G.emptyGraphic
drawDot (x, y) True =  G.regionToGraphic $ G.rectangleRegion (x+1, y+1) (x+3, y+3)
