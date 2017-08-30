
import Data.Char

halve :: [a] -> ([a],[a])
halve x = (take ((length x) `div` 2) x, drop ((length x) `div` 2) x)

thirdInd :: [a] -> a
thirdInd x = x !! 2

thirdHT :: [a] -> a
thirdHT x = head(tail(tail x))

thirdPM :: [a] -> a
thirdPM (a:b:x:_) = x

mhead :: [a] -> a
mhead [] = error "Error"
mhead (x:_) = x

safetailP :: [a] -> [a]
safetailP [] = []
safetailP (x:xs) = xs

safetailIF :: [a] -> [a]
safetailIF x = if null x then x else tail x

safetailG :: [a] -> [a]
safetailG x | null x = x
            | otherwise = tail x

myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x <-xs]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x, _)<-ps]

length':: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x |x <-[1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']


let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n))
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e|(o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode(-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

sumSqrs :: [Int]
sumSqrs = [x^2|x <- [1..100]]

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(x',y')|x'<-[0..x], y'<-[0..y]]

square :: Int -> [(Int, Int)]
square x = [(c,b)|(c,b) <- grid x x, (c,b) /= (b,c)]

replicate' :: Int -> a -> [a]
replicate' x y = [y | _ <- [1..x]]

pyths :: Int -> [(Int, Int, Int)]
pyths x = [(a,b,c)|a<-[1..x],b<-[1..x],c<-[1..x],(a^2 + b^2)==c^2]

listfacs :: Int -> [(Int,[Int])]
listfacs x = [(a,factors a) |a<-[1..x]]

perfects :: Int -> [Int]
perfects x = [a | (a, b) <- lsfac, a == sum(tail(reverse(b)))]
  where
    lsfac = [xs | xs <-listfacs x]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct x y = sum [ x'*y' | (x',y') <- zip x y]

fac :: Int -> Int
fac n = product [1..n]

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac'(n - 1)

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

length2 :: [a] -> Int
length2 [] = 0
length2 (_:xs) = 1 + length2 xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

insert:: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop (n-1) xs

fib' :: Int -> Int
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n-2) + fib'(n-1)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
      smaller = [a | a <- xs, a <= x]
      larger =  [b | b <-xs , b > x]

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)
