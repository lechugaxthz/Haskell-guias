module Guia5 where

-- ejercicio 1

-- a
longitud :: [t] -> Int
longitud (x : []) = 1
longitud (_ : xs) = 1 + longitud xs

-- b
ultimo :: [t] -> t
ultimo [x] = x
ultimo (_ : xs) = ultimo xs

-- c
{--principio :: [t] -> t
principio--}

-- d
reverso :: [t] -> [t]
reverso [x] = [x]
reverso (x : xs) = reverso xs ++ [x]

-- ejercicio 2

-- a
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x : xs) = e == x || pertenece e xs

-- b
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [x] = True
todosIguales (x : y : xs) = x == y && todosIguales (y : xs)

-- c
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [x] = False
todosDistintos (x : y : xs) = x /= y && todosDistintos (y : xs)

-- d
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [x] = False
hayRepetidos (x : xs) = pertenece x xs || hayRepetidos xs

-- e

-- aux
dif :: (Eq a) => a -> a -> Bool
dif n x = n /= x

quitar :: (Eq t) => t -> [t] -> [t]
quitar n [x]
  | dif n x = [x]
  | otherwise = []
quitar n (x : xs)
  | not (dif n x) = xs
  | otherwise = x : quitar n xs

-- f
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos n [] = []
quitarTodos n [x]
  | dif n x = [x]
  | otherwise = []
quitarTodos n (x : xs)
  | dif n x = x : quitar n xs
  | otherwise = quitar n xs

-- g
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x : xs) = x : eliminarRepetidos (quitarTodos x xs)

-- h
-- quedo fando falso en casos ciertos. revisar !!
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos _ [] = False
mismosElementos [] _ = False
mismosElementos (x : xs) ys = pertenece x ys && mismosElementos (quitarTodos x xs) (quitarTodos x ys)

{--
SALTAMOS AL EJERCICIO 3
--}

-- ejercicio 3

-- a
sumatoria :: [Integer] -> Integer
sumatoria = sum

-- b
productoria :: [Integer] -> Integer
productoria = product

-- c
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x : y : xs)
  | x > y = maximo (x : xs)
  | otherwise = maximo (y : xs)

-- d
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n (x : xs) = (n + x) : sumarN n xs

-- e
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x : xs) = sumarN x (x : xs)

-- f
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (head (reverso xs)) xs

-- g
pares :: [Integer] -> [Integer]
pares [] = []
pares (x : xs)
  | par = x : pares xs
  | otherwise = pares xs
  where
    par = even x

-- h
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x : xs)
  | mod x n == 0 = x : multiplosDeN n xs
  | otherwise = multiplosDeN n xs

-- i
ordenar :: (Ord t) => [t] -> [[t]]
ordenar [x] = [[x]]
ordenar [x, y]
  | x < y = [[x, y]]
  | otherwise = [[y, x]]
ordenar (x : y : xs) = [x, y] : segundoOrd
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

-- ejercicio 4

-- a
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x : y : xs)
  | x == y && x == ' ' = x : sacarBlancosRepetidos (x : xs)
  | otherwise = x : sacarBlancosRepetidos (y : xs)

{--
ME DA PAJA HACERLO!
YA LOS HICIMOS EN CLASES!!!!!!
--}

-- ejerciocio 5

-- a
sumaAcumulados :: (Num t) => [t] -> [t]
sumaAcumulados xs = reverso (sumaAcumuladosAux (reverso xs))

sumaAcumuladosAux :: (Num t) => [t] -> [t]
sumaAcumuladosAux [x] = [x]
sumaAcumuladosAux (x : xs) = sum (x : xs) : sumaAcumuladosAux xs

-- b
{--
SOLO PASA EL CASO DE EJEMPLO. TENDRIA QUE SUGERIR QUE SI NO LO RECONOCE COMOREACION ENTRE PRIMOS NO LO TOME!
--}
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x : xs) 
    | boolPrimo = [x] : descomponerEnPrimos xs
    | otherwise = descomponerAux x 2 : descomponerEnPrimos xs
    where 
      boolPrimo = isPrime x

-- aux
descomponerAux :: Integer -> Integer -> [Integer]
descomponerAux x n 
  | isPrime n && mod x n == 0 && isPrime (div x n) = [n, div x n]
  | otherwise = descomponerAux x (n+1)

isPrime :: Integer -> Bool
isPrime n = null [x | x <- [2..div n 2], mod n x == 0]

-- ejercicio 6

-- types
type Texto = [Char]

type Nombre = Texto
type Telefono = Texto

type Contacto = (Nombre, Telefono)

type ContactosTel = [Contacto]

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos nombre (contacto)

agregarContacto :: Nombre -> ContactosTel -> ContactosTel

eliminarContact :: Nombre -> ContactosTel -> ContactosTel

