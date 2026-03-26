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
potencia a = a * a * a

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
