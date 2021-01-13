-- Problem 357 : http://projecteuler.net/problem=357 

import Data.Numbers.Primes
import Data.Set (fromList, member)

n = 100000000
primes' = takeWhile (<n) primes
firstPrimes = fromList (primes')
isPrime' x
	| x < n = x `member` firstPrimes
	| otherwise = isPrime x
	
sqr n = round . sqrt $ fromIntegral n
	
divisors n = 1 + n : 2 + n `div` 2 : [d + x | x <- [3.. sqr n+1], let (d,m) = n `divMod` x, d > x, m == 0 ]
check n = all isPrime' $ divisors n
main = print $ sum $ filter check (map (\x -> x - 1) primes')