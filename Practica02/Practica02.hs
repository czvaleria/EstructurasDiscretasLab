-- I. Listas con recursión.
-- 1. Obtener la longitud de una lista.
longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

-- 2. Sumar todos los n ́umeros de una lista.
sumaLista :: Num a => [a] -> a 
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista (xs)

-- 3. Agregar un elemento a una lista.
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento lista elemento True = elemento:lista 
agregaElemento lista elemento False = lista ++ [elemento]

-- 4. Obtener el máximo de una lista.
maximoLista :: (Ord a, Num a) => [a] -> a
maximoLista [x] = x
maximoLista (x:xs) = comparar x (maximoLista xs)

-- Función auxiliar para comparar.
comparar :: Ord a => a -> a -> a
comparar x y = if x > y 
    then x 
    else y

-- 5. Recuperar un elemento de una lista de acuerdo a su índice.
recuperarElemento :: [a] -> Int -> a
recuperarElemento [] ind = error "Uy, índice no válido :/"
recuperarElemento (x:xs) 0 = x
recuperarElemento (x:xs) ind = recuperarElemento xs (ind - 1)

-- II. Listas por comprehensión.
-- 1. Obtener la lista con n elementos de los divisiores del entero n
divisoresDeN :: Int -> [Int]
divisoresDeN n = [x | x <- [1..n], n `mod` x == 0]

-- 2. Convertir una lista a conjunto.
conjuntoLista :: Eq a => [a] -> [a]
conjuntoLista xs = [x | (x, ys) <- repetidos xs, estaEn x ys == False]

-- Función auxiliar para ver quienes son los repetidos
repetidos :: [a] -> [(a, [a])]
repetidos [] = []
repetidos (x:xs) = (x, xs) : repetidos xs

-- Función auxiliar para saber si hay repetidos y no incluirlos
estaEn :: Eq a => a -> [a] -> Bool
estaEn z [] = False
estaEn z (x:xs) = z == x || estaEn z xs

-- 3. Obtener los números pares de una lista.
soloPares :: [Int] -> [Int]
soloPares lista = [x | x <- lista, x `mod` 2 == 0]