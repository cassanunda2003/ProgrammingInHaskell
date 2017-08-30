double x = x + x
quadruple x = double (double x)
factorial n = product [1 .. n]
average ns = sum ns `div` length ns

a = b + c
    where
        b = 1
        c = 2

d = a * 2

avg = a `div` length xs
    where
        a = 15
        xs = [1,2,3,4,5]

add :: (Int, Int) -> Int
add (x,y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0 .. n]

add' :: Int -> (Int -> Int)
add' x y = x + y

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

abs' :: Int -> Int
abs' n | n >= 0     = n
       | otherwise  = -n

signum' :: Int -> Int
signum' n | n < 0       = -1
          | n == 0      = 0
          | otherwise   = 1

not' :: Bool -> Bool
not' False = True
not' True  = False

and :: Bool -> Bool -> Bool
and True True   = True
and _  _         = False

test :: [Char] -> Bool
test ['a',_,_]  = True
test _          = False

test' :: [Char] -> Bool
test' ('a':_)   = True
test' _         = False

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs