-- ====================
--      UTILIDADES  
-- ==================== 
-- maxi function
maxi a b
  | a < b = b
  | otherwise = a

-- mini function
mini a b
  | a < b = a
  | otherwise = b

-- es_primo
es_primo n
  | n < 3 = n > 1
  | (length [x | x <- [3 .. n-1], mod n x == 0]) > 0 = False
  | otherwise = True

-- Arbol
data AB t = 
  A (AB t) t (AB t)
  | V deriving Show

-- sub arbol izquierdo
izq :: AB a -> AB a
izq (A l n r) = l

-- sub arbol derecho
der :: AB a -> AB a
der (A l n r) = r 

-- Arbol de ejemplo
ab = 
  A (
      A (A V 2 V) 
      5 
      (A V 7 V)
    )
    8 
    (
      A V 
      9 
      (
        A (A V 11 V)
        15
        V
       )
    )

-- ====================
--    EJERCICIOS
-- ====================

-- medio calcula el promedio del mayor y menor de entre 4 argumentos
medio a b c d = (maxi (maxi a b) (maxi c d) + mini (mini a b) (mini c d)) / 2

-- primos
primos a b 
  | (a < b) && (es_primo a) = (1 + (primos (a + 1) b))
  | (a < b) = (0 + (primos (a + 1) b))
  | (a == b) && (es_primo b) = 1
  | otherwise = 0

-- mayores
mayores :: [Int] -> [Int] -> [Int]
mayores [] y = []
mayores x [] = []
mayores (x:xs) (y:ys)
  | y <= x = 1 : mayores xs ys
  | otherwise = 2 : mayores xs ys

-- multiplica
-- multiplica :: [Int] -> [Int] -> [Int]
-- multiplica [] y = []
-- multiplica x [] = []
-- multiplica x y  = 

-- desplaza
desplaza :: [Int] -> Int -> [Int]
desplaza a 0 = a
desplaza a n
  | n == 1 = last a : init a
  | otherwise = desplaza (last a : init a) (n - 1)

-- impares
impares V = []
impares (A l n r)
  | (mod n 2) /= 0 = [n] ++ (impares l) ++ (impares r)
  | otherwise = (impares l) ++ (impares r)

-- g_distintos

-- c_tabla
c_tabla n = [((n, x), (x*n)) | x <- [1..10]]

-- f_prodpar
f_prodpar l = map (foldl (*) 1) (filter (odd . length) l)

main = do
  -- print(maxi 1 2)
  -- print(mini 1 2)
  -- print(medio 2 1 5 4)
  -- print(medio 2 2 2 2)
  -- print(primos 1 10)
  -- print(primos 5 11)
  -- print(primos 8 10)
  -- print(mayores [8,5,2,4] [1,2,3,4])
  -- print(mayores [1,2,3] [2,3,1])
  -- print(multiplica[1,1,1] [])
  -- print(desplaza [1,2,3] 1)
  -- print(desplaza [1,2,3] 2)
  -- print(desplaza [1,2,3] 6)
  -- print(impares V)
  -- print(impares ab)
  -- print(impares (A (A (A V 3 V) 2 V) 1 (A (A V 5 V) 4 V)))
  -- print(c_tabla 1)
  -- print(c_tabla 8)
  print(f_prodpar [[1,2,3],[4,5],[6,7]])
  print(f_prodpar [[1],[1,2],[1,2,3],[4,3,2]])