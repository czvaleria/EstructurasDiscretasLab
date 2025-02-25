-- 1. Distancia entre dos puntos en el plano cartesiano.
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- 2. Valor absoluto de un número.
valorAbsoluto :: Int -> Int
valorAbsoluto n = if n < 0
                then (n) * (-1)
                else n

-- 3. Pendiente de la recta que pasa por dos puntos.
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

-- 4. Hipotenusa de un triangulo rectángulo.
hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt (b^2 + h^2)

-- 5. Raíces de una ecuación cuadrática.
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = if (sqrt(b^2 - 4*a*c) <= 0)
    then (0/0, 0/0)
    else ( (-b - sqrt (b^2 - 4*a*c)) / (2*a), (-b + sqrt (b^2 - 4*a*c)) / (2*a) )


-- 6. Area de un triángulo por medio de la fórmula de Herón.
-- Función auxiliar para calcular el semiperímetro
se :: Float -> Float -> Float -> Float
se a b c = (a + b + c) / 2

-- Función para calcular el área de un triángulo usando la fórmula de Herón
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (se a b c * (se a b c - a) * (se a b c - b) * (se a b c - c))

-- 7. Determinar si un año es bisiesto.
esBisiesto :: Int -> Bool
esBisiesto a = mod a 4 == 0

-- 8. Función comparador
comparador :: Int -> Int -> Int
comparador x y =
                if x > y then
                   1
                else if x == y then
                   0
                else
                  -1

-- 9. Máximo entre tres números
maximo :: Int -> Int -> Int -> Int
maximo x y z = if x > y 
    then (if x > z 
        then x 
        else z) 
    else (if y > z 
        then y 
        else z)


-- 10. Verificar si los números están ordenados de forma descendente
esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w =
    if x >= y
    then (if y >= z
          then (if z >= w
                then True
                else False)
          else False)
    else False