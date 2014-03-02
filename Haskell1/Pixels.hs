{-|
Module      : Pixels
Description : Módulo que permite pasar un texto y transformarlo en Pixeles
                de LED Displays.
Copyright   : Daniela Rodríguez, 2014
              Patrick Rengifo, 2014
-}
module Pixels
(font,
pixelsToString,
pixelListToPixels,
pixelListToString,
concatPixels,
messageToPixels,
up,
down,
left,
right,
upsideDown,
backwards,
negative,
)
where

import Data.Char (ord)
import Data.Bits(testBit,Bits)
import Data.List(transpose)

-- | 'fontBitmap' es el diccionario de caracteres escritos en enteros hexadecimales
fontBitmap :: [[Int]]
fontBitmap =
  [
    [ 0x00, 0x00, 0x00, 0x00, 0x00 ], --  (Space)
    [ 0x00, 0x00, 0x5F, 0x00, 0x00 ], --  !
    [ 0x00, 0x07, 0x00, 0x07, 0x00 ], --  "
    [ 0x14, 0x7F, 0x14, 0x7F, 0x14 ], --  #
    [ 0x24, 0x2A, 0x7F, 0x2A, 0x12 ], --  $
    [ 0x23, 0x13, 0x08, 0x64, 0x62 ], --  %
    [ 0x36, 0x49, 0x55, 0x22, 0x50 ], --  &
    [ 0x00, 0x05, 0x03, 0x00, 0x00 ], --  '
    [ 0x00, 0x1C, 0x22, 0x41, 0x00 ], --  (
    [ 0x00, 0x41, 0x22, 0x1C, 0x00 ], --  )
    [ 0x08, 0x2A, 0x1C, 0x2A, 0x08 ], --  *
    [ 0x08, 0x08, 0x3E, 0x08, 0x08 ], --  +
    [ 0x00, 0x50, 0x30, 0x00, 0x00 ], --  ,
    [ 0x08, 0x08, 0x08, 0x08, 0x08 ], --  -
    [ 0x00, 0x60, 0x60, 0x00, 0x00 ], --  .
    [ 0x20, 0x10, 0x08, 0x04, 0x02 ], --  /
    [ 0x3E, 0x51, 0x49, 0x45, 0x3E ], --  0
    [ 0x00, 0x42, 0x7F, 0x40, 0x00 ], --  1
    [ 0x42, 0x61, 0x51, 0x49, 0x46 ], --  2
    [ 0x21, 0x41, 0x45, 0x4B, 0x31 ], --  3
    [ 0x18, 0x14, 0x12, 0x7F, 0x10 ], --  4
    [ 0x27, 0x45, 0x45, 0x45, 0x39 ], --  5
    [ 0x3C, 0x4A, 0x49, 0x49, 0x30 ], --  6
    [ 0x01, 0x71, 0x09, 0x05, 0x03 ], --  7
    [ 0x36, 0x49, 0x49, 0x49, 0x36 ], --  8
    [ 0x06, 0x49, 0x49, 0x29, 0x1E ], --  9
    [ 0x00, 0x36, 0x36, 0x00, 0x00 ], --  :
    [ 0x00, 0x56, 0x36, 0x00, 0x00 ], --  ;
    [ 0x00, 0x08, 0x14, 0x22, 0x41 ], --  <
    [ 0x14, 0x14, 0x14, 0x14, 0x14 ], --  =
    [ 0x41, 0x22, 0x14, 0x08, 0x00 ], --  >
    [ 0x02, 0x01, 0x51, 0x09, 0x06 ], --  ?
    [ 0x32, 0x49, 0x79, 0x41, 0x3E ], --  @
    [ 0x7E, 0x11, 0x11, 0x11, 0x7E ], --  A
    [ 0x7F, 0x49, 0x49, 0x49, 0x36 ], --  B
    [ 0x3E, 0x41, 0x41, 0x41, 0x22 ], --  C
    [ 0x7F, 0x41, 0x41, 0x22, 0x1C ], --  D
    [ 0x7F, 0x49, 0x49, 0x49, 0x41 ], --  E
    [ 0x7F, 0x09, 0x09, 0x01, 0x01 ], --  F
    [ 0x3E, 0x41, 0x41, 0x51, 0x32 ], --  G
    [ 0x7F, 0x08, 0x08, 0x08, 0x7F ], --  H
    [ 0x00, 0x41, 0x7F, 0x41, 0x00 ], --  I
    [ 0x20, 0x40, 0x41, 0x3F, 0x01 ], --  J
    [ 0x7F, 0x08, 0x14, 0x22, 0x41 ], --  K
    [ 0x7F, 0x40, 0x40, 0x40, 0x40 ], --  L
    [ 0x7F, 0x02, 0x04, 0x02, 0x7F ], --  M
    [ 0x7F, 0x04, 0x08, 0x10, 0x7F ], --  N
    [ 0x3E, 0x41, 0x41, 0x41, 0x3E ], --  O
    [ 0x7F, 0x09, 0x09, 0x09, 0x06 ], --  P
    [ 0x3E, 0x41, 0x51, 0x21, 0x5E ], --  Q
    [ 0x7F, 0x09, 0x19, 0x29, 0x46 ], --  R
    [ 0x46, 0x49, 0x49, 0x49, 0x31 ], --  S
    [ 0x01, 0x01, 0x7F, 0x01, 0x01 ], --  T
    [ 0x3F, 0x40, 0x40, 0x40, 0x3F ], --  U
    [ 0x1F, 0x20, 0x40, 0x20, 0x1F ], --  V
    [ 0x7F, 0x20, 0x18, 0x20, 0x7F ], --  W
    [ 0x63, 0x14, 0x08, 0x14, 0x63 ], --  X
    [ 0x03, 0x04, 0x78, 0x04, 0x03 ], --  Y
    [ 0x61, 0x51, 0x49, 0x45, 0x43 ], --  Z
    [ 0x00, 0x00, 0x7F, 0x41, 0x41 ], --  [
    [ 0x02, 0x04, 0x08, 0x10, 0x20 ], --  \
    [ 0x41, 0x41, 0x7F, 0x00, 0x00 ], --  ]
    [ 0x04, 0x02, 0x01, 0x02, 0x04 ], --  ^
    [ 0x40, 0x40, 0x40, 0x40, 0x40 ], --  _
    [ 0x00, 0x01, 0x02, 0x04, 0x00 ], --  `
    [ 0x20, 0x54, 0x54, 0x54, 0x78 ], --  a
    [ 0x7F, 0x48, 0x44, 0x44, 0x38 ], --  b
    [ 0x38, 0x44, 0x44, 0x44, 0x20 ], --  c
    [ 0x38, 0x44, 0x44, 0x48, 0x7F ], --  d
    [ 0x38, 0x54, 0x54, 0x54, 0x18 ], --  e
    [ 0x08, 0x7E, 0x09, 0x01, 0x02 ], --  f
    [ 0x08, 0x14, 0x54, 0x54, 0x3C ], --  g
    [ 0x7F, 0x08, 0x04, 0x04, 0x78 ], --  h
    [ 0x00, 0x44, 0x7D, 0x40, 0x00 ], --  i
    [ 0x20, 0x40, 0x44, 0x3D, 0x00 ], --  j
    [ 0x00, 0x7F, 0x10, 0x28, 0x44 ], --  k
    [ 0x00, 0x41, 0x7F, 0x40, 0x00 ], --  l
    [ 0x7C, 0x04, 0x18, 0x04, 0x78 ], --  m
    [ 0x7C, 0x08, 0x04, 0x04, 0x78 ], --  n
    [ 0x38, 0x44, 0x44, 0x44, 0x38 ], --  o
    [ 0x7C, 0x14, 0x14, 0x14, 0x08 ], --  p
    [ 0x08, 0x14, 0x14, 0x18, 0x7C ], --  q
    [ 0x7C, 0x08, 0x04, 0x04, 0x08 ], --  r
    [ 0x48, 0x54, 0x54, 0x54, 0x20 ], --  s
    [ 0x04, 0x3F, 0x44, 0x40, 0x20 ], --  t
    [ 0x3C, 0x40, 0x40, 0x20, 0x7C ], --  u
    [ 0x1C, 0x20, 0x40, 0x20, 0x1C ], --  v
    [ 0x3C, 0x40, 0x30, 0x40, 0x3C ], --  w
    [ 0x44, 0x28, 0x10, 0x28, 0x44 ], --  x
    [ 0x0C, 0x50, 0x50, 0x50, 0x3C ], --  y
    [ 0x44, 0x64, 0x54, 0x4C, 0x44 ], --  z
    [ 0x00, 0x08, 0x36, 0x41, 0x00 ], --  {
    [ 0x00, 0x00, 0x7F, 0x00, 0x00 ], --  |
    [ 0x00, 0x41, 0x36, 0x08, 0x00 ]  --  }
  ]

type Pixels = [String]

-- | Transformamos el caracter dado, buscamos su numero asci y con eso
-- lo buscamos en fontBitmap
lookupLetter :: Char -> [Int]
lookupLetter letter
          | pos>=94 || pos < 0 = [0xFF,0xFF,0xFF,0xFF]
          | otherwise = (fontBitmap !! pos)
          where pos = ord letter - 32


-- | Con un index, chequeamos cual es True y cual es False. Colocando
-- los * y ' ' respectivos.
transform :: Bits a => a -> String
transform x = map toChar [0..6]
  where toChar y = if testBit x y
                   then '*'
                   else ' '

-- | Invertimos los elementos en la lista, y usamos los procedimientos
-- anteriores para imprimir la secuencia correspondiente a el caracter
-- introducido.
font :: Char  -> Pixels
font letter = transpose $ map transform (lookupLetter letter)

-- |unlines toma una lista de cadenas y las une utilizando un '\n'
pixelsToString :: Pixels -> String
pixelsToString = unlines

-- | Unimos todos los elementos de la lista de entrada con una cadena
-- vacia entre ellos
pixelListToPixels :: [Pixels] -> Pixels
pixelListToPixels = foldr(\x y -> x ++ [""] ++ y) []

-- | Hacemos composición de funciones. Primero pixelsListToPixels
-- para tener un solo pixel, y luego pixelsToString.

pixelListToString :: [Pixels] -> String
pixelListToString = pixelsToString . pixelListToPixels


-- | Como list es una lista de listas,unimos las listas internas con
-- zipWith, y luego con foldr | unimos todo en un solo Pixels
concatPixels :: [Pixels] -> Pixels
concatPixels list = foldr (zipWith(++)) ["", "", "", "", "", "", "", ""] list

-- | Mapeamos todo el string introducido con font para tenerlos en tipo
-- pixel. Unimos con foldr y zipWith usando whiteSpace, que agrega el espacio
-- en blanco entre las letras a imprimir.
messageToPixels :: String -> Pixels
messageToPixels = foldr (zipWith whiteSpace)
                       ["", "", "", "", "", "", "", ""]
                       . map font
  where whiteSpace x y = x ++ [' '] ++ y


-- Efectos especiales

-- | A la cola de pixeles le adjuntamos la 'cabeza' de la misma al final
up :: Pixels -> Pixels
up (x:xs) = xs ++ [x]

-- | Al último elemento de los pixels, le adjuntamos el resto de la lista
down :: Pixels -> Pixels
down x = last x:(init x)

-- | Mapeamos al pixels con la función move, que coloca la 'cabeza' al
-- final del string.
left :: Pixels -> Pixels
left x = map move x
  where move(x:xs) = xs ++[x]

-- | Igual que con left, mapeamos colocando el último elemento al principio
right :: Pixels -> Pixels
right = map (\x -> last x:(init x))

-- | Colocamos al revés el pixels con reverse.
upsideDown :: Pixels -> Pixels
upsideDown = reverse

-- | Mapeamos cada elemento con reverse.
backwards :: Pixels -> Pixels
backwards = map reverse

-- | Mapeamos el mapeo con la función x, que invierte asteriscos por espacios
negative :: Pixels -> Pixels
negative = map (map x)
  where x '*' = ' '
        x ' ' = '*'
