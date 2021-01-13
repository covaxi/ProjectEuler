-- Problem 131 : http://projecteuler.net/problem=131 

import Data.Numbers.Primes

main = print $ length $ takeWhile (<10^6) [p | x <- [1..], let p = 3 * x * (x + 1 ) + 1, isPrime p]