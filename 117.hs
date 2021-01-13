-- Problem 117 : http://projecteuler.net/problem=117 

f 0 = 1
f 1 = 1
f 2 = f 0 + f 1
f 3 = f 0 + f 1 + f 2
f 4 = f 0 + f 1 + f 2 + f 3
f x = ff !! (x-1) + ff !! (x-2) + ff !! (x-3) + ff !! (x-4)

ff = map f [0..]

main = print $ f 50