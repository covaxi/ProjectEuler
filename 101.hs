-- Problem 101 : http://projecteuler.net/problem=101 

import Data.Ratio

arr::[Integer]
arr =  map (\n -> 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10) [0..10]
-- arr = map (\n->n^3) [0..3]

f :: Integer -> Int -> Ratio Integer
f 0 x = (arr !! 1) % 1
f 1 x = (arr !! (x+1)) % 1 - (arr !! x ) % 1
f n x = (f (n-1) (x+1) - f (n-1) x) / (n % 1)


fi 0 x = f 0 x
fi n x = product [toInteger x % 1 - i % 1 | i <- [1 .. n]] * f n 1

ff n x = sum [g | nn <- [0..n], let g = fi nn x]

bop n = ff n (fromIntegral (n+2))

main = print $ numerator $ sum $ map bop [0..9]