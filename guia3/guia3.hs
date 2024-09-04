module Guia3 where

-- ejercicio 1

--
f :: Int -> Int
f x
  | x == 1 = 8
  | x == 4 = 131
  | x == 16 = 16
  | otherwise = 0

g :: Int -> Int
g x
  | x == 8 = 16
  | x == 16 = 4
  | x == 131 = 1
  | otherwise = 0

h :: Int -> Int
h x = f (g x)

k :: Int -> Int
k x = g (f x)

-- ejercicio 2

-- a
abs1 :: Int -> Int
abs1 x
  | x > (-1) = x
  | x < 0 = -x

-- b
maxAbs :: Int -> Int -> Int
maxAbs x y
  | abs1 x > abs1 y = x
  | otherwise = y

-- c
max3 :: Int -> Int -> Int -> Int
max3 x y z
  | (x >= y) && (x >= z) = x
  | (y >= x) && (y >= z) = y
  | otherwise = z

-- d
algunoEs0 :: Int -> Int -> Bool
algunoEs0 x y
  | (x == 0) || (y == 0) = True
  | otherwise = False

algunoEs0Path :: Int -> Int -> Bool
algunoEs0Path 0 y = True
algunoEs0Path x 0 = True
algunoEs0Path x y = False

-- e
ambosSon0 :: Int -> Int -> Bool
ambosSon0 x y
  | x == 0 && x == y = True
  | otherwise = False

ambosSon0Path :: Int -> Int -> Bool
ambosSon0Path 0 0 = True
ambosSon0Path x y = False

-- f
mismoIntervalo :: Double -> Double -> Bool
mismoIntervalo x y
  | x == y = True
  | (x <= 3) && (y <= 3) = True
  | (x > 7) && (y > 7) = True
  | (x <= 7) && (x > 3) && (y <= 7) && (y > 3) = True
  | otherwise = False

-- g
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z
  | (x == y) && (x == z) = x
  | (x == y) || (x == z) = y + z
  | y == z = x + z
  | otherwise = x + y + z

-- h
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

-- i
digitoUnidades :: Int -> Int
digitoUnidades x
  | x >= 0 = mod x 10
  | otherwise = mod (-x) 10

-- j
digitoDecenas :: Int -> Int
digitoDecenas x
  | x < 10 = 0
  | otherwise = div (mod (abs x) 100 - mod (abs x) 10) 10

-- ejercicio 3

estanRelacionados :: Int -> Int -> Bool
estanRelacionados x y
  | x == y && x == 0 = False
  | otherwise = mod x y == 0

-- ejercicio 4

-- types
type VectorR2 = (Double, Double)

type VectorR3 = (Double, Double, Double)

type VectorR3Int = (Int, Int, Int)

-- a
prodInt :: VectorR2 -> VectorR2 -> Double
prodInt (x, y) (w, z) = x * w + y * z

-- b
todoMenor :: VectorR2 -> VectorR2 -> Bool
todoMenor (x, y) (w, z) = (x < w) && (y < z)

-- c
distanciaPuntos :: VectorR2 -> VectorR2 -> Double
distanciaPuntos (x, y) (w, z) = sqrt (handlerCuadradoBase + handlerCuadradoAltura)
  where
    handlerCuadradoBase = (x * w) ^ 2
    handlerCuadradoAltura = (y * z) ^ 2

-- d
sumaTerna :: VectorR3 -> Double
sumaTerna (x, y, z) = x + y + z

-- e
sumarSoloMultiplos :: VectorR3Int -> Int -> Int
sumarSoloMultiplos (x, y, z) n = xn + yn + zn
  where
    xn
      | mod x n == 0 = x
      | otherwise = 0
    yn
      | mod y n == 0 = y
      | otherwise = 0
    zn
      | mod z n == 0 = z
      | otherwise = 0

-- f
posPrimerPar :: VectorR3Int -> Int
posPrimerPar (x, y, z)
  | evenX && evenY && evenZ = 4
  | evenX = 1
  | evenY = 2
  | evenZ = 3
  | otherwise = 0
  where
    evenX = even x
    evenY = even y
    evenZ = even z

-- aburre xdddddd

-- ejercicio 5
todosMenores :: VectorR3Int -> Bool
todosMenores (x, y, z) = xfg && yfg && zfg
  where
    xfg = f x > g x
    yfg = f y > g y
    zfg = f z > g z

f5 :: Int -> Int
f5 n
  | n <= 7 = n ^ 2
  | otherwise = 2 * n - 1

g5 :: Int -> Int
g5 n
  | even n = div n 2
  | otherwise = 3 * n + 1

-- ejercicio 6

-- types
type Anio = Int

type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto año = (mod año 4 == 0) || (mod año 100 == 0 && mod año 400 /= 0)