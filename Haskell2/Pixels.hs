{-|
Module      : Pixels
Description : Módulo que permite pasar un texto y transformarlo en Pixeles
                de LED Displays.
Copyright   : Daniela Rodríguez, 2014
              Patrick Reunify, 2014
-}

module Pixels
       (
         readFont,
         messageToPixels,
         font,
         Pixels(..),
         Pixel(..)
       -- , font
       ) where

import System.IO
import qualified Data.Map as M
import Control.Monad

data Pixels = Pixels { -- color :: Color
                     {- ,  -} dots ::[[Pixel]] }
           deriving (Show)

data Pixel = Pixel { on :: Bool } deriving Show

-- | Recibe el Map y la letra a convertir. Si esta no se encuentra, se selecciona
-- el primer elemento del Map para crear un Pixels todo prendido.
font :: M.Map Char Pixels -> Char -> Pixels
font dicc wanted = if (M.member wanted dicc)
                   then dicc M.! wanted
                   else let x = snd (M.elemAt 0 dicc)
                            n = length (dots x) -- filas
                            m = length ((dots x) !! 0) -- columnas
                        in Pixels [[Pixel True | y <- [1..m]] | x <- [1..n]]


pixelListToPixels :: [Pixels] -> Pixels
pixelListToPixels xs = Pixels { dots = newDots }
  where newDots = concatMap dots xs


-- pixelListToPixels = undefined
concatPixels :: [Pixels] -> Pixels
concatPixels xs = Pixels {dots = newDots} where
  newDots = foldr (zipWith (++)) initial $ initialDots
  initialDots = map dots xs
  initial = if null initialDots
            then []
            else [[] | x <- [1..(length (initialDots !! 0))] ]

messageToPixels :: M.Map Char Pixels ->  String -> Pixels
messageToPixels m xs = Pixels {dots = newDots} where
  newDots = foldr (zipWith (addWhitespace)) initial $ initialDots
  initialDots = map dots $ map (font m) xs
  initial = if null initialDots
            then []
            else [[] | x <- [1..(length (initialDots !! 0))] ]
  addWhitespace x y = x ++ [whitespace] ++ y
  whitespace = Pixel { on = False }

-- up = undefined
-- down = undefined
-- left = undefined
-- right = undefined
-- upsideDown = undefined
-- backwards = undefined
-- negative = undefined

-- | Este maldito crea una lista de cada letra a representar con su font
partition :: [a] -> Int -> [[a]]
partition [] _ = []
partition list n = take n list : partition (drop n list) n

-- | Este maldito chequea que la puta letra esté bien representada
-- entre las putas comillas
readLetter('"':letter:'"':xs) = letter
readLetter x = error $  "Formato incorrecto especificando una letra." ++ (show x)

-- | Este desgraciado que todos los peroles del font sean asteriscos
-- o espacios en blanco. BECAUSE FUN FUN FUN LOOKING FOWARD TO THE
-- WEEEEEEKEND.
readLetterRep = map (\x -> if check x
                           then x
                           else error "Caracteres inesperados")

-- | Este hace lo mismo del anterior.
check = all inRange
  where inRange x = x == '*' || x == ' '

checkSize :: Int -> Int -> [String] -> [String]
checkSize row col list = if (length list) /= row+1
                         then error "No furulan las filas"
                         else (head list) : map (\x -> if length x /= col

                                         then error "No furulan las columnas."
                                         else x ) (tail list)


-- | Convertimos all shit en pixels. Tiramos todo en un huge map... A.K.A un DICCIONARIO
readEachLetter :: Int -> Int -> [[String]] -> M.Map Char Pixels -> IO (M.Map Char Pixels)
readEachLetter _ _ [] accum = return accum
readEachLetter n m letters accum = do
  let (letterCod:current) = checkSize n m  (head letters)
      letter = readLetter letterCod
      newAccum = M.insert letter pixels accum
      pixels = Pixels { dots = map transform (readLetterRep current) }
      transform x = map transformChar x
      transformChar '*' = Pixel { on = True}
      transformChar ' ' = Pixel { on = False }
  readEachLetter n m (tail letters)  newAccum


-- | This bad boy hace magia. Te verifica si están los putos números de
-- filas y columnas, te lee el archivo y toda la paja.

readFont :: Handle-> IO (M.Map Char Pixels)
readFont h = do
  contents <- hGetContents h
  let l = lines contents
      numbers = words (head l)
  if length numbers /= 2
    then error "No hay docs números."
    else do let col =  read (numbers !! 0) :: Int
                row = read (numbers !! 1) :: Int
                letters = partition (tail l) (row+1)
            readEachLetter row col letters M.empty
