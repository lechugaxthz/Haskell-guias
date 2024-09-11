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

sumaImparesHandler :: Int-> Int -> Int
sumaImparesHandler x 1 = 1
sumaImparesHandler x y = sumaImparesHandler x (y-1) 