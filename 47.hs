-- Problem 47 : http://projecteuler.net/problem=47 

import Data.List

isPrime n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes
				
primes = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs
 
primeFactors n 	| n > 1 = go n primes
				| otherwise = []
   where
     go n ps@(p:ps')
        | p*p > n        = [n]
        | n `rem` p == 0 =  p : go (n `quot` p) ps
        | otherwise      =      go n ps'
		
npf = nub . primeFactors
npfl = length . npf

main = print $ head [x |
	x <- [2*3*5*7..],
	npfl x == 4,
	npfl (x+1) == 4,
	npfl (x+2) == 4,
	npfl (x+3) == 4]