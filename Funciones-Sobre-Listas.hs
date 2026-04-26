--Parte 2
tercero :: [a] -> a
tercero xs = head (tail (tail xs))

segundo :: [a] -> a
segundo xs = head (tail xs)

listaFuncion xss a = (head (head xss)) a 

tercerElemento xss = quinto (tercero xss) 
                where quinto xs = head (tail (tail (tail (tail xs))))

tercerElemento1 xss = segundo (cuarta (tercero xss))
                where cuarta xs = head (tail (tail (tail xs) ))

