import Data.Char

--Create synonym for bit
type Bit = Int

--Convert a list of bits to an Int
bin2int' :: [Bit] -> Int
bin2int' bits = sum [w*b | (w,b) <- zip weights bits]
  where
    weights = iterate (*2) 1


--Using algebra it is possible to express the function using foldr
--Note that the bits are written in reverse order
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

--Convert int to bits using multiple division
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
