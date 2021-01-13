-- Problem 73 : http://projecteuler.net/problem=73 

main = print $ sum [ 1 | d <- [1..12000], n <- [d `div` 3 - 1..d `div` 2 + 1], gcd n d == 1, n * 3 > d, n * 2 < d] 