-- Problem 123 : http://projecteuler.net/problem=123 

import Data.Numbers.Primes

result = head [n | (n,p) <- zip [1..] primes, odd n, 2*n*p > 10^10]