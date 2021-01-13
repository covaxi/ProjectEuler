-- Problem 137 : http://projecteuler.net/problem=137 

-- http://oeis.org/A081018
fibs = 1:1:zipWith (+) fibs (tail fibs)

main = print $ (fibs !! 29) * (fibs !! 30)