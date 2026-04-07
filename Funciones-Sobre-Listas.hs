--Parte 2
tercero :: [a] -> a
tercero xs = head (tail (tail xs))

segundo :: [a] -> a
segundo xs = head (tail xs)