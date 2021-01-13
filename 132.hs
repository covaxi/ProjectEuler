-- Problem 132 : http://projecteuler.net/problem=132 

import Data.Numbers.Primes

modPow :: Integral a => a -> a -> a -> a
modPow 0 _ _ = 0
modPow 1 _ _ = 1
modPow _ 0 _ = 1
modPow _ _ 0 = 0
modPow _ _ 1 = 0
modPow b e m = case (e `mod` 2) of { 0 -> modPow (b*b `mod` m) (e `div` 2) m; 1 -> (b * modPow (b*b `mod` m) (e `div` 2) m) `mod` m; }

testP = (==1) . modPow 10 (10^9) . (*9)

main = print $ sum $ take 40 $ filter testP primes
