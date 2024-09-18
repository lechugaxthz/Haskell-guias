module Guia5 where

-- ejercicio 1

-- a
longitud :: [t] -> Int
longitud (x:[]) = 1
longitud (_:xs) = 1 + longitud xs

-- b