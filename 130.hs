-- Problem 130 : http://projecteuler.net/problem=130 

import Data.Numbers.Primes

a n
    | gcd n 10 /= 1 = 0
    | otherwise = 1 + (length $ takeWhile (/= 0) $ iterate (\x -> (x * 10 + 1) `mod` n) 1)


main = print $ sum $ take 25 [x | x <- [3,5..], gcd x 10 == 1, not $ isPrime x, (x-1) `mod` (a x) == 0]