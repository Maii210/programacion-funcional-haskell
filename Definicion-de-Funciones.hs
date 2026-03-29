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

--Ejercicio 3 - Distincion de casos
mayorDe4 :: Int -> Int -> Int -> Int -> Int
mayorDe4 a b c d | mayorde2 a b > mayorde2 c d = mayorde2 a b
                 | otherwise = mayorde2 c d
            
nota2 :: Int -> String
nota2 n | n >= 51 && n <= 100 = "Aprobado"
        | n < 51 = "Reprobado"
        | otherwise = "Nota inválida"

nota3 :: Int -> String
nota3 n | n >= 90 && n <= 100 = "Excelente"
        | n >= 70 && n <= 89 = "Bien"
        | n >= 51 && n <= 69 = "Regular"
        | n >= 0 && n <= 50 = "Mal"
        | otherwise = "Nota inválida"

nota4 :: Int -> Int -> Int -> Int -> String
nota4 p1 p2 ef si | promedio (fromIntegral p1) (fromIntegral p2) >= 51 || ef >= 51 || si >= 51 = "Aprobado"
                   | promedio (fromIntegral p1) (fromIntegral p2) == 0 && ef == 0 && si == 0 = "Abandono"
                   | otherwise = "Reprobado"

mayorde16 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
mayorde16 a b c d e f g h i j k l m n o p | mayorde4 a b c d > mayorde4 e f g h && mayorde4 a b c d > mayorde4 i j k l && mayorde4 a b c d > mayorde4 m n o p = mayorde4 a b c d
                                  | mayorde4 e f g h > mayorde4 a b c d && mayorde4 e f g h > mayorde4 i j k l && mayorde4 e f g h > mayorde4 m n o p = mayorde4 e f g h
                                  | mayorde4 i j k l > mayorde4 a b c d && mayorde4 i j k l > mayorde4 e f g h && mayorde4 i j k l > mayorde4 m n o p = mayorde4 i j k l
                                  | otherwise = mayorde4 m n o p

quebrado :: Int -> Int -> Bool
quebrado num den | prom > 1 = True
                  | otherwise = False
                  where prom = div num den

fecha2 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fecha2 (d1, m1, a1) (d2, m2, a2) | a1 > a2 = (d1, m1, a1)
                                  | a2 > a1 = (d2, m2, a2)
                                  | m1 > m2 = (d1, m1, a1)
                                  | m2 > m1 = (d2, m2, a2)
                                  | d1 > d2 = (d1, m1, a1)
                                  | otherwise = (d2, m2, a2)
                          
aniosTranscurridos :: (Int, Int, Int) -> (Int, Int, Int) -> Int
aniosTranscurridos (d1, m1, a1) (d2, m2, a2) | m1 > m2 || (m1 == m2 && d1 > d2) = a2 - a1 - 1
                                               | otherwise = a2 - a1

mesesTranscurridos :: (Int, Int, Int) -> (Int, Int, Int) -> Int
mesesTranscurridos (d1, m1, a1) (d2, m2, a2) | d1 > d2 = (a2 - a1) * 12 + (m2 - m1) - 1
                                               | otherwise = (a2 - a1) * 12 + (m2 - m1)

diasTranscurridos :: (Int, Int, Int) -> (Int, Int, Int) -> Int
diasTranscurridos (d1, m1, a1) (d2, m2, a2) | m1 > m2 || (m1 == m2 && d1 > d2) = (a2 - a1 - 1) * 365 + (m2 - m1 + 12) * 30 + (d2 - d1)
                                               | otherwise = (a2 - a1) * 365 + (m2 - m1) * 30 + (d2 - d1)                                               

diasmesesyanios :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
diasmesesyanios (d1, m1, a1) (d2, m2, a2) = (diasTranscurridos (d1, m1, a1) (d2, m2, a2), mesesTranscurridos (d1, m1, a1) (d2, m2, a2), aniosTranscurridos (d1, m1, a1) (d2, m2, a2))

instante :: (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
instante (d, m, a, h, min, s) | s < 59 = (d, m, a, h, min, s + 1)
                              | min < 59 = (d, m, a, h, min + 1, 0)
                              | h < 23 = (d, m, a, h + 1, 0, 0)
                              | d < diasEnMes m a = (d + 1, m, a, 0, 0, 0)
                              | m < 12 = (1, m + 1, a, 0, 0, 0)
                              | otherwise = (1, 1, a + 1, 0, 0, 0)
                              where diasEnMes mes anio | mes == 2 && esBisiesto anio = 29
                                                  | mes == 2 = 28
                                                  | mes `elem` [1, 3, 5, 7, 8, 10, 12] = 31
                                                  | otherwise = 30
                                    esBisiesto anio | (anio `mod` 4 == 0 && anio `mod` 100 /= 0) || (anio `mod` 400 == 0) = True
                                                    | otherwise = False     

--Ejercicio 4 - Usando case
vocal :: Char -> Char
vocal x = case x of 'a' -> 'e'
                    'e' -> 'i'
                    'i' -> 'o'
                    'o' -> 'u'
                    'u' -> 'a'
                    _   -> error "No es una vocal"

digito :: Int -> String
digito x = case x of 0 -> "Cero"
                     1 -> "Uno"
                     2 -> "Dos"
                     3 -> "Tres"
                     4 -> "Cuatro"
                     5 -> "Cinco"
                     6 -> "Seis"
                     7 -> "Siete"
                     8 -> "Ocho"
                     9 -> "Nueve"
                     _ -> error "No es un dígito"
             

andOp :: Int -> Int -> Bool
andOp x y = case (x, y) of (0, 0) -> False
                           (0, 1) -> False
                           (1, 0) -> False
                           _ -> True

orOp :: Int -> Int -> Bool
orOp x y = case (x, y) of (0, 0) -> False
                          (0, 1) -> True
                          (1, 0) -> True
                          _ -> True

xorOp :: Int -> Int -> Bool
xorOp x y = case (x, y) of (0, 0) -> False
                           (0, 1) -> True
                           (1, 0) -> True
                           _ -> False 

andorxor :: Int -> Int -> (Int -> Int -> Bool) -> Bool
andorxor x y f = f x y 

literalTres :: Int -> String
literalTres x = case (div x 100, div (mod x 100) 10, mod x 10) of
  (1, 0, 0) -> "Cien"
  (1, d, u) -> "Ciento " ++ literalDosDigitos (d * 10 + u)
  (c, 0, 0) -> digito c ++ "cientos"
  (c, d, u) -> digito c ++ "cientos " ++ literalDosDigitos (d * 10 + u)
  _ -> error "No es un número de tres dígitos"

literalDosDigitos :: Int -> String
literalDosDigitos x = case (div x 10, mod x 10) of
  (1, 0) -> "Diez"
  (1, u) -> "Diez y " ++ digito u
  (2, 0) -> "Veinte"
  (2, u) -> "Veinte y " ++ digito u
  (3, 0) -> "Treinta"
  (3, u) -> "Treinta y " ++ digito u
  (4, 0) -> "Cuarenta"
  (4, u) -> "Cuarenta y " ++ digito u
  (5, 0) -> "Cincuenta"
  (5, u) -> "Cincuenta y " ++ digito u
  (6, 0) -> "Sesenta"
  (6, u) -> "Sesenta y " ++ digito u
  (7, 0) -> "Setenta"
  (7, u) -> "Setenta y " ++ digito u
  (8, 0) -> "Ochenta"
  (8, u) -> "Ochenta y " ++ digito u
  (9, 0) -> "Noventa"
  (9, u) -> "Noventa y " ++ digito u
  (0, u) -> if u == 0 then "" else digito u
  _ -> error "No es un número de dos dígitos"

numeroMenor :: Int -> Int -> Int -> Int -> Int -> Int -> Int
numeroMenor a b c d e f = case (a, b, c, d, e, f) of
  (x, y, z, w, v, u) | x <= y && x <= z && x <= w && x <= v && x <= u -> x
                     | y <= x && y <= z && y <= w && y <= v && y <= u -> y
                     | z <= x && z <= y && z <= w && z <= v && z <= u -> z
                     | w <= x && w <= y && w <= z && w <= v && w <= u -> w
                     | v <= x && v <= y && v <= z && v <= w && v <= u -> v
                     | otherwise -> f

