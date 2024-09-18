module Guia4 where

-- ejercicio 1
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

{-
fib 5
    fib 4 + fib 3
        fib 3 + fib 2 + fib 2 + fib 1
            fib 2 + fib 1 + fib 1 + fib 0 + fib 1 + fib 0 + 1
                fib 1 + fib 0 + 1 + 1 + 0 + 1 + 0 + 1
                1 + 0 + 1 + 1 + 0 + 1 + 0 + 1
-}

-- ejercicio 2
parteEntera :: Float -> Int
parteEntera = floor

-- ejercicio 3
esDivisible :: Int -> Int -> Bool
esDivisible x y
  | x == y = True
  | y <= 0 || x < 0 || x < y = False
  | otherwise = esDivisible (x - y) y

-- ejercicio 4
sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares x = x

sumaImparesHandler :: Int -> Int -> Int
sumaImparesHandler x 1 = 1
sumaImparesHandler x y = sumaImparesHandler x (y - 1)

-- ejercicio 5
medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact x = x * medioFact (x - 2)

-- ejercicio 6
todosDigitosIguales :: Int -> Bool
todosDigitosIguales x
  | x < 10 = True
  | otherwise = (first == second) && todosDigitosIguales (div x 10)
  where
    first = mod x 10
    second = mod (div x 10) 10

-- ejercicio 7
iesimoDigito :: Int -> Int -> Int
iesimoDigito x 1 = mod x 10
iesimoDigito x y = iesimoDigito (div x 10) (y - 1)

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
  | x == y = 2 ^ y
  | otherwise = 2 ^ y + g1 x (y + 1)

f2 :: Int -> Float -> Float
f2 n m = g2 n 1 m

g2 :: Int -> Int -> Float -> Float
g2 x y z
  | x == y = z ^ y
  | otherwise = z ^ y + g2 x (y + 1) z

f3 :: Int -> Float -> Float
f3 n m = g2 (n * 2) 1 m

f4 :: Float -> Float -> Float
f4 1 1 = 2
f4 n 1 = 2 * n + 1 - n
f4 1 q = (q ^ (2 - 1) - q) / (q - 1)

-- ejercicio 11
-- a
factorial :: (Num a, Enum a, Eq a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

eAprox :: (Fractional a, Enum a, Eq a) => a -> a
eAprox 0 = 1
eAprox n = (1 / factorial n) + eAprox (n - 1)

-- b
e :: Float
e = eAprox 10

-- ejercicio 12
raizDe2Aprox :: (Fractional a, Enum a, Eq a) => a -> a -> a
raizDe2Aprox 0 x = x
raizDe2Aprox n x = raizDe2Aprox (n - 1) (0.5 * (x + 2 / x))

-- ejercicio 13
f13 :: (Integral a, Fractional b) => a -> a -> b
f13 1 m = fromIntegral m
f13 n m = (fromIntegral (n ^ (m + 1) - 1) / fromIntegral (n - 1)) + f13 (n - 1) m

{--
Pasamos alejercicio 14 en clase
--}

-- ejercicio 14
{--
sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias (x : N, y : N, z : N) : N {
    require : {x,y,z >= 0}
    resolve :{res == q^(a+b) "/" (1 <= a && 1 <= y) && (1 <= b && 1 <= z )}
}
--}

sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias q a b = sumaPotenciasHandler q a b b

sumaPotenciasHandler :: Int -> Int -> Int -> Int -> Int
sumaPotenciasHandler x valorActualY valorInicialZ valorActualZ
  | ambos1 = x ^ 2
  | zMayorQue1 = x ^ sumaValoresActuales + sumaPotenciasHandler x valorActualY valorInicialZ (valorActualZ - 1)
  | otherwise = x ^ sumaValoresActuales + sumaPotenciasHandler x (valorActualY - 1) valorInicialZ valorInicialZ
  where
    ambos1 = valorActualY == valorActualZ && valorActualY == 1
    zMayorQue1 = valorActualZ > 1
    sumaValoresActuales = valorActualY + valorActualZ

{--
Está mal pero se puede  hacer por estelado tambien!!!!
--}
sumaPotencias2 :: Int -> Int -> Int -> Int
sumaPotencias2 q a b
  | q == 1 = potencia - 1
  | otherwise = div (q ^ (potencia + 1) - q - q ^ 2 - 1) (q - 1)
  where
    potencia = a + b

-- ejercicio 15
sumaRacionales :: (Integral a, Fractional b) => a -> a -> b
sumaRacionales n 1 = fromIntegral (n * (n + 1)) / 2
sumaRacionales n m = (fromIntegral (n * (n + 1)) / 2) * (1 / fromIntegral m) + sumaRacionales n (m - 1)

-- lo dejamos para despues!!!

-- ejercicio 16

-- a
menorDivisor :: Int -> Int
menorDivisor x
  | mod x 2 == 0 = 2
  | mod x 3 == 0 = 3
  | otherwise = menorDivisorAux x 5 1

menorDivisorAux :: Int -> Int -> Int -> Int
menorDivisorAux x y z
  | x <= y = x
  | mod y 2 == 0 = menorDivisorAux x (y + (3 * z)) (z + 1)
  | mod x y == 0 = y
  | y < x = menorDivisorAux x (y + (3 * z)) (z + 1)

{--
Saltamos al ejercicio 19
--}

-- ejercicio 19

{--esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n =

esSumaInicialDePrimosAux :: Int -> Int -> Bool
esSumaInicialDePrimosAux n m
    | m > n = False
    | m == n = True
    | mod m 2 /= 0 = esSumaInicialDePrimosAux n (m+2)
    | mod m 3 == --}
