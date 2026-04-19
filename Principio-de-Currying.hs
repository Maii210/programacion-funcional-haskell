f1 :: Bool -> Int -> Int -> Int
f1 x y z = if x then y + 10 else z

f2 :: Int -> Int -> Char -> Int
f2 x y z = if z=='s' then 2*x else y

f3 :: (Int -> Bool) -> (Char -> Bool) -> (Bool -> Bool) -> Bool
f3 x y z = (x 2) && (y 'a') && (z True)

f4 :: (Int -> Bool) -> Int -> Int -> Int -> Int
f4 x y z w = if x 2 then z else w+10

f5 :: (ta -> Int, ta) -> (Int -> Int, Int) -> Int
f5=(\(x,y) -> \(z ,w) -> (x y)+ (z w))

f6 :: tx -> tx
f6 x = x

f7 :: (tx -> ty) -> tx -> ty
f7 x y = x y

f8 :: (tx -> ty) -> (tz -> tx) -> tz -> ty
f8 x y z = x (y z)

f9 :: (tx -> ty -> tz) -> tx -> ty -> tz
f9 x y z = (x y) z

f10 :: (tx -> ty) -> (tz -> tx) -> (tw -> tz) -> tw -> ty
f10 x y z w= x (y (z w))

f11 :: (tx -> ty -> tz -> tw) -> tx -> ty -> tz -> tw
f11 x y z w= ((x y) z) w

f12 :: (a -> b -> c) -> a -> (d -> b) -> d -> c
f12 x y z w= ((x y) (z w))

f13 :: Int -> Int -> tz -> Int
f13 x y z = x*2+y

f14 :: (tx -> ty -> tz) -> tx -> ty -> tz
f14 x y z = x y z

f15 :: Bool -> Bool -> Bool -> Bool
f15 x y z | x = y
	        | y = z

f16 :: (tx -> tx -> ty) -> (tz -> tx) -> tz -> ty
f16 x y z = x (y z)(y z)

c :: (a -> b -> c) -> (a, b) -> c 
c f (x, y) = f x y

u :: ((a, b) -> c) -> a -> b -> c 
u f x y = f(x, y)

f17 :: tx -> Int -> (Int -> Int) -> Int
f17 x y z = s + y
				where s = z y

f18 :: (Int -> Int -> Int) -> Int -> Int -> Int
f18 x y z = r1 + r2
  where
    r1 = x 5 y
    r2 = x y z
		
f19 :: ((a, b) -> c) -> a -> b -> c
f19 f = g
	where g x y = f (x, y)

f20 :: (a -> b -> c) -> (a, b) -> c
f20 f = g
      where g (x,y) = f x y
  
f21 :: ((a, b, c) -> d) -> a -> b -> c -> d
f21 f = g
      where g x y z = f(x, y, z)

f22 :: Bool -> Bool -> (Bool -> Bool) -> Bool -> a -> Bool
f22 e x y z w = if x && (y x) then z else w z
          where w a |a = e
                    |otherwise = z

curry1 :: ((a, b) -> c) -> a -> b -> c
curry1 f x y = f(x,y)

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = g
       where g x y = f(x,y)