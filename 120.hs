-- Problem 120 : http://projecteuler.net/problem=120 

rm a = 2 * a * ((a - 1) `div` 2)

main = print $ sum $ map rm [3..1000]