-- Problem 87 : http://projecteuler.net/problem=87 
import Data.Numbers.Primes
import Data.List
import Data.Set

m = 50000000
primes2 = takeWhile (<m) [x*x | x <- primes]
primes3 = takeWhile (<m) [x*x*x | x <- primes]
primes4 = takeWhile (<m) [x*x*x*x | x <- primes]
sums = fromList [a+b+c | a <- primes4, b <- primes3, a+b <= m, c <- primes2, a+b+c <= m] 

main = print $ length $ elems sums