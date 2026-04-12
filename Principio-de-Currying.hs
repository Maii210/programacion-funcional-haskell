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

u f x y = f(x, y)

