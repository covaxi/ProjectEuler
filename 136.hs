-- Problem 136 : http://projecteuler.net/problem=136 

import Data.Numbers.Primes

main = let m = 5*10^7 in print $ sum [length $ filter
  (\p -> mod (p+1) 4 == 0) $ takeWhile (<m) primes,
  length $ takeWhile (< div m  4) primes,
  length $ takeWhile (< div m 16) primes]