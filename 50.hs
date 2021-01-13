-- Problem 50 : http://projecteuler.net/problem=50 
import Data.Ord
import Data.List

isPrime n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes
 
primeFactors n | n > 1 = go n primes
   where
     go n ps@(p:ps')
        | p*p > n        = [n]
        | n `rem` p == 0 =  p : go (n `quot` p) ps
        | otherwise      =      go n ps'

primes = 2 : filter isPrime [3,5..]

s x y = sum (take x (drop y primes))

  
main = print $ maximumBy (comparing snd) [(z,x) |
	x <- [22..600],
	y <- [1..600],
	let z = s x y,
	z < 1000000,
	isPrime z]