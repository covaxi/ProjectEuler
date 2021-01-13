-- Problem 28 : http://projecteuler.net/problem=28 

main = print $ 1 + sum [f x | x <- [3,5..1001]] where  f x = x * x * 4 - (x-1) * 6
