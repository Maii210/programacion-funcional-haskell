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

sumatoria :: Int -> Int -> Int -> String
sumatoria a b c = case (a + b + c) of
                    suma | suma < 10 -> "Sumatoria menor"
                         | suma < 20 -> "Sumatoria mayor"
                         | otherwise -> "Vacio"

notas5 :: Int -> Int -> Int -> String
notas5 n1 n2 n3 = 
  let prom = promedio (fromIntegral n1) (fromIntegral n2) + fromIntegral n3 / 3
  in case prom of
       p | p >= 90 && p <= 100 -> "Excelente"
         | p >= 70 && p < 90 -> "Bien"
         | p >= 51 && p < 70 -> "Regular"
         | p >= 0 && p < 51 -> "Mal"
         | otherwise -> "Nota inválida" 

--Ejercicio 5 - Definiciones locales
menorDe6 :: Int -> Int -> Int -> Int -> Int -> Int -> Int
menorDe6 a b c d e f = if menorDe3 a b c < menorDe3 d e f then menorDe3 a b c else menorDe3 d e f where menorDe3 x y z = if x < y && x < z then x else if y < z then y else z

sumatoria2 :: Int -> Int -> Int -> String
sumatoria2 a b c = if suma < 10 then "Sumatoria menor" else if suma < 20 then "Sumatoria mayor" else "Vacio" where suma = a + b + c

notas6 :: Int -> Int -> Int -> String
notas6 a b c = if promedio >= 90 && promedio <= 100 then "Excelente" 
                      else if promedio >= 70 && promedio < 90 then "Bien" 
                          else if promedio >= 51 && promedio < 70 then "Regular" 
                              else "Mal" where promedio = ((fromIntegral a) + (fromIntegral b) + (fromIntegral c)) / 3

--Ejercicio 6 - Reconocimiento de patrones
fechaDia :: (Int, Int, Int) -> Int
fechaDia (d, _, _) = d

fechaMes :: (Int, Int, Int) -> Int
fechaMes (_, m, _) = m

fechaAño :: (Int, Int, Int) -> Int
fechaAño (_, _, a) = a

quebradoMayor :: (Int, Int) -> (Int, Int) -> (Int, Int)
quebradoMayor (num1, den1) (num2, den2) | num1 * den2 > num2 * den1 = (num1, den1)
                                           | otherwise = (num2, den2)

quebradoReducido :: (Int, Int) -> (Int, Int)
quebradoReducido (num, den) = let divisor = gcd num den in (num `div` divisor, den `div` divisor)

quebradoSigno :: (Int, Int) -> Char
quebradoSigno (num, den) | num * den > 0 = '+'
                          | num * den < 0 = '-'
                          | otherwise = '0'

fechaMenor :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fechaMenor fe1@(d1, m1, a1) fe2@(d2, m2, a2) fe3@(d3, m3, a3) = 
  let fechas = [fe1, fe2, fe3]
  in foldl (\f1 f2 -> if fechaAnterior f1 f2 then f1 else f2) (head fechas) (tail fechas)
  where
    fechaAnterior (d1, m1, a1) (d2, m2, a2) = 
      if a1 < a2 then True
      else if a1 > a2 then False
      else if m1 < m2 then True
      else if m1 > m2 then False
      else d1 < d2

horaMayor :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
horaMayor ho1@(h1, min1, s1) ho22@(h2, min2, s2) | h1 > h2 = ho1
                             | h2 > h1 = ho2
                             | min1 > min2 = ho1
                             | min2 > min1 = ho2
                             | s1 > s2 = ho1
                             | otherwise = ho2

instanteMayor :: (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
instanteMayor i1@(d1, m1, a1, h1, min1, s1) i2@(d2, m2, a2, h2, min2, s2) | a1 > a2 = i1
                             | a2 > a1 = i2
                             | m1 > m2 = i1
                             | m2 > m1 = i2
                             | d1 > d2 = i1
                             | d2 > d1 = i2
                             | h1 > h2 = i1
                             | h2 > h1 = i2
                             | min1 > min2 = i1
                             | min2 > min1 = i2
                             | s1 > s2 = i1
                             | otherwise = i2

siguiente :: Int -> Int
siguiente n = n + 1

simplificarQuebrado :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
simplificarQuebrado ((a, b), (c, d)) = let num = a * d
                                            den = b * c
                                            divisor = gcd num den
                                        in ((num `div` divisor, den `div` divisor), (1, 1))
                              