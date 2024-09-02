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
    | abs1 (x) > abs1 (y) = x 
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

--e
ambosSon0 :: Int -> Int -> Bool
ambosSon0 x y 
    | x == 0 && x == y = True
    | otherwise = False 

ambosSon0Path :: Int -> Int -> Bool
ambosSon0Path 0 0 = True
ambosSon0Path x y = False

--f
mismoIntervalo :: Double -> Double -> Bool
mismoIntervalo x y 
    | (x == y) = True
    | (x <= 3) && (y <= 3) = True
    | (x > 7) && (y > 7) = True
    | (x <= 7) && (x > 3) && (y <= 7) && (y > 3) = True
    | otherwise = False

-- g 
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z 
    | (x == y) && (x == z) = x
    | (x == y) || (x == z)  = y + z
    | (y == z) = x + z
    | otherwise = x + y + z

-- h
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

-- i
digitoUnidades :: Int -> Int
digitoUnidades x 
    | x >= 0 = mod x 10
    | otherwise = mod (-x) 10

--j
digitoDecenas :: Int -> Int 
digitoDecenas x 
    | x < 10 = 0
    | otherwise = div (mod (abs x) 100 - mod (abs x) 10) 10

-- ejercicio 3
algoc x y =  [x^2 + x*y*k == 0| k towards (-10) 10 ]

--estanRelacionados :: Int -> Int -> Bool
--estanRelacionados x y 
    -- | x == y && x == 0 = False
    -- | otherwise = map  {x^2 + x*y*k == 0| k=[(-10).. 10] } 