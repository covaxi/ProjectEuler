-- Problem 41 : http://projecteuler.net/problem=41 

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
		
fld = foldl1 ( \x y -> x*10 + y)

digitize' _ 0 = []
digitize' y x = mod x y : digitize' y (div x y)
dig x = reverse $ digitize' 10 x

main = print $ maximum $ filter (isPrime) (map (fld) (permutations [1..7]))