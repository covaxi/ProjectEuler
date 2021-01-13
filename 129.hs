-- Problem 129 : http://projecteuler.net/problem=129 


import Data.List

a n
    | gcd n 10 /= 1 = 0
    | otherwise = 1 + (length $ takeWhile (/= 0) $ iterate (\x -> (x * 10 + 1) `mod` n) 1)

main = print $ head $ [x | x <- [10^6..], (a x) > 10^6]