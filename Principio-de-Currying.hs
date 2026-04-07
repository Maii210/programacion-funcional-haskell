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