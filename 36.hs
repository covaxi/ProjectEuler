-- Problem 36 : http://projecteuler.net/problem=36 

digitize _ 0 = []
digitize y x = mod x y : digitize y (div x y)

pal y x = digitize y x == (reverse $ digitize y x)

main = print $ sum [x | x <-[1,3..999999], pal 2 x, pal 10 x]