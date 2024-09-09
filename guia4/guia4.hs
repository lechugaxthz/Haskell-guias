module Guia4 where

-- ejercicio 5
medioFact :: Int -> Int 
medioFact 0 = 1
medioFact 1 = 1
medioFact x = x * medioFact (x-2)


-- ejercicio 6
todosDigitosIguales :: Int -> Bool
todosDigitosIguales x 
    | x<10 = True
    | otherwise = (first == second) && todosDigitosIguales (div x 10)
    where 
        first = mod x 10
        second = mod (div x 10) 10

-- ejercicio 7 
iesimoDigito :: Int -> Int -> Int
iesimoDigito x 1 = mod x 10
iesimoDigito x y = iesimoDigito(div x 10) (y-1)

-- ejercicio 8
sumaDigitos :: Int -> Int 
sumaDigitos x 
    | x < 10 = x
    | otherwise = mod x 10 + sumaDigitos (div x 10)

-- ejercicio 9
-- REHACER CUANDO TENGAS TODO LO DE LA OTRA PC
esCapicua :: Int -> Bool
esCapicua x = True

-- ejercicio 10
f1 :: Int -> Int
f1 n = g1 n 0

g1 :: Int -> Int -> Int
g1 x y 
    | x == y = 2^y
    | otherwise = 2^y + g1 x (y + 1)

f2 :: Int -> Double -> Double
f2 n m = g2 n 1 m

g2 :: Int -> Int -> Double -> Double
g2 x y z 
    | x == y = z^y
    | otherwise = z^y + g2 x (y+1) z

f3 :: Int -> Double -> Double
f3 n m = g2 (n*2) 1 m

{--
f4 :: Int -> Int
--}