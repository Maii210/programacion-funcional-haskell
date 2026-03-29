--Ejercicio 1
areaCuadrado :: Int -> Int
areaCuadrado lado = lado * lado

areaRectangulo :: Int -> Int -> Int
areaRectangulo base altura = base * altura
perimetroRectangulo :: Int -> Int -> Int 
perimetroRectangulo base altura = 2 * (base + altura)

numeroMayor :: Int -> Int -> Bool
numeroMayor a b = if a > b then True else False

multiplodeDos :: Int -> Bool
multiplodeDos a = if mod a 2 == 0 then True else False 

multiplodeDosyTres :: Int -> Bool
multiplodeDosyTres a = if mod a 2 == 0 && mod a 3 == 0 then True else False

potencia :: Int -> Int
potencia a = a * a

potencia4 :: Int -> Int
potencia4 a = a * a * a * a

potencia8 :: Int -> Int
potencia8 a = potencia4 a * potencia4 a

potencia10 :: Int -> Int
potencia10 a = a ^ 10

potencia32 :: Int -> Int
potencia32 a = a ^ 32

verificarOrden :: (Int -> Int -> Bool) -> Int -> Int -> Bool
verificarOrden f x y = f x y

--Ejercicio 2 - Expresiones if
mayorde2 :: Int -> Int -> Int
mayorde2 a b = if a > b then a else b

mayorde3 :: Int -> Int -> Int -> Int
mayorde3 a b c = if a > b && a > c then a else if b > c then b else c

mayorde4 :: Int -> Int -> Int -> Int -> Int
mayorde4 a b c d = if mayorde2 a b > mayorde2 c d then mayorde2 a b else mayorde2 c d

promedio :: Float -> Float -> Float
promedio p1 p2 = (p1 + p2) / 2

nota :: Int -> Int -> Int -> Int -> String
nota p1 p2 ef si = if promedio (fromIntegral p1) (fromIntegral p2) >= 51 || ef >= 51 || si >= 51 then "Aprobado" else if promedio (fromIntegral p1) (fromIntegral p2) == 0 && ef == 0 && si == 0 then "Abandono" else "Reprobado"

fechaMayor :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fechaMayor (d1, m1, a1) (d2, m2, a2) = if a1 > a2 then (d1, m1, a1) else if a2 > a1 then (d2, m2, a2) else if m1 > m2 then (d1, m1, a1) else if m2 > m1 then (d2, m2, a2) else if d1 > d2 then (d1, m1, a1) else (d2, m2, a2)