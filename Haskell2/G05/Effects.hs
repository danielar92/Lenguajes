{-|
Module      : Effects
Description : Módulo de implementación de los efectos de los pixeles
Copyright   : Daniela Rodríguez, 2014
              Patrick Rengifo, 2014
-}

module Effects
       (
         Effects(..),
         readEffects,
         isFinite,
         calcRunning
       ) where

import System.IO
import Data.Char
import qualified Graphics.HGL as G

-- | Data Effects que contiene los efectos aplicables a los Pixels.
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



-- | Atrapamos errores en la lectura del archivo de efectos
--  https://stackoverflow.com/questions/5121371/how-to-catch-a-no-parse-exception-from-the-read-function-in-haskell
readEffects ::  String -> Effects
readEffects s = case filter (null . dropWhile isSpace . snd) (reads s) of
    [(x, _)] -> x
    y         -> error "No pude parsear el archivo de efectos."

-- | Calculamos cuando tarda en correr la lista de Effects. Solo funciona si no hay Forever
calcRunning :: [Effects] -> Integer
calcRunning = foldl calc 0
  where
    calc x (Delay y) = x + y
    calc x (Repeat y xs) = x + y*calcRunning xs
    calc x _ = x

-- | Verificamos si la lista de Effects es finita. Si lo es, retornamos True, en caso contrario False.
isFinite :: [Effects] -> Bool
isFinite = all isFinite'
  where isFinite' (Forever _) = False
        isFinite' (Repeat _ xs) = isFinite xs
        isFinite' _ = True
