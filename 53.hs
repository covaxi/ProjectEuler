-- Problem 53 : http://projecteuler.net/problem=53 

c x y = product [x-y+1..x] `div` product [1..y]
main = print $ length [ 1 | x <- [1..100], y <- [1..x], c x y > 1000000] where 
