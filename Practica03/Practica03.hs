data Arbol a = ArbolBinarioVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-- 1. Obtener la longitud de un Árbol Binario.
longitud :: Arbol a -> Int
longitud ArbolBinarioVacio = 0
longitud (Raiz r izquierdo derecho) = 1 + longitud izquierdo + longitud derecho

-- 2. Obtener la profundidad de un Árbol Binario.
profundidad :: Arbol a -> Int
profundidad ArbolBinarioVacio = 0
profundidad (Raiz r izquierdo derecho) = 1 + max (profundidad izquierdo) (profundidad derecho)

-- 3.Obtener el ancho de un Árbol Binario.
ancho :: Arbol a -> Int
ancho ArbolBinarioVacio = 0
ancho (Raiz r izquierdo derecho) = 1 + ancho izquierdo + ancho derecho

-- 4. Recorridos de un Árbol Binario.
data Recorrido = InOrder | PreOrder | PosOrder
recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolBinarioVacio x = []

-- 4.1 InOrden
recorrido (Raiz r izquierdo derecho) InOrder = recorrido izquierdo InOrder ++ [r] ++ recorrido derecho InOrder

-- 4.2 PreOrden
recorrido (Raiz r izquierdo derecho) PreOrder = [r] ++ recorrido izquierdo PreOrder ++ recorrido derecho PreOrder

-- 4.3 PostOrden
recorrido (Raiz r izquierdo derecho) PosOrder = recorrido izquierdo PosOrder ++ recorrido derecho PosOrder ++ [r]

-- 5. Obtener elemento de un Árbol Binario según su nivel.
niveles :: Arbol a -> [[a]]
niveles ArbolBinarioVacio = []
niveles (Raiz r izquierdo derecho) = [r] : obtenerNivel (niveles izquierdo) (niveles derecho)

-- Función auxiliar para obtener los niveles de los subárboles
obtenerNivel :: [[a]] -> [[a]] -> [[a]]
obtenerNivel [] ys = ys
obtenerNivel xs [] = xs
obtenerNivel (x:xs) (y:ys) = (x ++ y) : obtenerNivel xs ys

-- 6. Obtener el minímo de un Árbol Binario.
minimo :: Ord a => Arbol a -> a
minimo ArbolBinarioVacio = error "No tiene minímo, es un árbol vacío"
minimo (Raiz r ArbolBinarioVacio ArbolBinarioVacio) = r
minimo (Raiz r izquierdo ArbolBinarioVacio) = if r < minimo izquierdo
                                                then r 
                                                else minimo izquierdo
minimo (Raiz r ArbolBinarioVacio derecho) = if r < minimo derecho
                                                then r 
                                                else minimo derecho
minimo (Raiz r izquierdo derecho) = if r <= minimo izquierdo && r <= minimo derecho
                                        then r
                                        else if minimo izquierdo <= minimo derecho
                                                then minimo izquierdo 
                                                else minimo derecho

-- 7. Obtener el máximo de un Árbol Binario.
maximo :: Ord a => Arbol a -> a
maximo ArbolBinarioVacio = error "No tiene máximo, es un árbol vacío"
maximo (Raiz r ArbolBinarioVacio ArbolBinarioVacio) = r
maximo (Raiz r izquierdo ArbolBinarioVacio) = if r > maximo izquierdo
                                                then r 
                                                else maximo izquierdo
maximo (Raiz r ArbolBinarioVacio derecho) = if r > maximo derecho
                                                then r
                                                else maximo derecho
maximo (Raiz r izquierdo derecho) = if r >= maximo izquierdo && r >= maximo derecho
                                        then r
                                        else if maximo izquierdo >= maximo derecho
                                                then maximo izquierdo
                                                else maximo derecho

-- 8. Eliminar un elemento de un Árbol Binario ordenado.
eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolBinarioVacio x = error "No existe, es un árbol vacío"
eliminar (Raiz r ArbolBinarioVacio ArbolBinarioVacio) x = if r == x 
                                                        then ArbolBinarioVacio 
                                                        else Raiz r ArbolBinarioVacio ArbolBinarioVacio
eliminar (Raiz r ArbolBinarioVacio derecho) x = if x < r
                                                    then Raiz r ArbolBinarioVacio (eliminar derecho x)
                                                    else if x > r
                                                        then Raiz r ArbolBinarioVacio derecho
                                                        else derecho
eliminar (Raiz r izquierdo ArbolBinarioVacio) x = if x > r
                                                    then Raiz r (eliminar izquierdo x) ArbolBinarioVacio
                                                    else if x < r
                                                        then Raiz r izquierdo ArbolBinarioVacio
                                                        else izquierdo
eliminar (Raiz r izquierdo derecho) x = if x < r
                                            then Raiz r (eliminar izquierdo x) derecho
                                            else if x > r
                                                then Raiz r izquierdo (eliminar derecho x)
                                                else Raiz (minimo derecho) izquierdo (eliminar derecho (minimo derecho))
                                                
-- Para este último caso, si quisieramos verificar que si es un árbol binario ordenado y hacer más robusto al programa, 
-- podriamos proceder como sigue, sin embargo, para el ejercicio suponemos que si es un árbol binario ordenado correcto.
-- eliminar (Raiz r izquierdo derecho) x = if x < r 
--                                        then Raiz r (eliminar izquierdo x) derecho
--                                            else if x > r 
--                                                then Raiz r izquierdo (eliminar derecho x)
--                                                else -- caso cuando x == r (encontramos a quién buscabamos)
--                                                    if esVacio izquierdo  
--                                                        then derecho
--                                                        else if esVacio derecho 
--                                                            then izquierdo
--                                                            else -- - caso cuando le damos la paternidad a uno de los hijos, osea el nodo que queremos eliminar tiene hijos
--                                                                Raiz (minimo derecho) izquierdo (eliminar derecho (minimo derecho)) 
-- Función auxiliar para ver si un subárbol está vacío
-- esVacio :: Arbol a -> Bool
-- esVacio ArbolBinarioVacio = True
-- esVacio x = False 

