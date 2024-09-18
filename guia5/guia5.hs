module Guia5 where

-- ejercicio 1

-- a
longitud :: [t] -> Int
longitud (x:[]) = 1
longitud (_:xs) = 1 + longitud xs

-- b
ultimo :: [t] -> t
ultimo [x] = x
ultimo (_:xs) = ultimo xs

-- c
{--principio :: [t] -> t
principio--}

-- d 
reverso :: [t] -> [t]
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

-- ejercicio 2

-- a
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

-- b 
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [x] = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs)

-- c
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [x] = False
todosDistintos (x:y:xs) = x /= y && todosDistintos (y:xs)

-- d 


-- e

-- aux
dif :: (Eq a) => a -> a -> Bool
dif n x = n/= x

quitar :: (Eq t) => t -> [t] -> [t]
quitar n [x]
    | dif n x  = [x]
    | otherwise = []

quitar n (x:xs)
    | not (dif n x) = xs
    | otherwise = x : quitar n xs

-- f
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos n [] = []
quitarTodos n [x]
        | dif n x = [x]
        | otherwise = []



quitarTodos n (x:xs)
    | dif n x = x : quitar n xs
    | otherwise = quitar n xs

-- g
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)


{--
SALTAMOS AL EJERCICIO 3
--}

-- ejercicio 3


-- c
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs)
    | x > y = maximo (x:xs)
    | otherwise = maximo (y:xs)

-- d 
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n (x:xs) = (n+x): sumarN n xs

-- e
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- f
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (head (reverso xs )) xs

-- g
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)
    | par = x : pares xs
    | otherwise = pares xs
    where
        par = even x

{--
paso al 9
--}

-- i
ordenar :: Ord t => [t] -> [[t]]
ordenar [x] = [[x]]
ordenar [x, y]
    | x < y = [[x,y]]
    | otherwise = [[y,x]]
ordenar (x:y:xs) = [x, y]: segundoOrd
    where
        segundoOrd = ordenar xs

{--
ordenAux :: [[t]] -> [t]
ordenAux ((x:xs):xss)

ordenAux ((x:xs):(y1,y2:ys):xss)

ordenAux ((x1:x2:xs):(y1:y2:ys):xss) 
    | mayores1 && mayores2 && x2 >= y1 = [x1,x2,y1,y2] 
    | mayores1 && x2 <= y1 = [x1,y1,x2,y2] 
    | x1 > y2 = [y1,x1,y2,x2]
    | otherwise = [y1,y2,x1,x2]
    where
        mayores1 = x1 > y1
        mayores2 = x2 > y2

TD _ [] = True

TD n (x:xs) = x == n && TD n xs 


[1,1,2,2,2,1,1]
1 1 [2,2,2,1,1]

--}