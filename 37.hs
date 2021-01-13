-- Problem 37 : http://projecteuler.net/problem=37 
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
		


digitize' _ 0 = []
digitize' y x = mod x y : digitize' y (div x y)
dig x = reverse $ digitize' 10 x

rights [x] = []
rights x = tail x : (rights $ tail x) 

lefts [x] = []
lefts x = init x : (lefts $ init x)

ccl x = lefts x ++ [x] ++ rights x

fld = foldl1 ( \x y -> x*10 + y)

start = [2,3,5,7]
to_add = [[1],[3],[7],[9]]

ends 0 = []
ends n = (to_add !! (n `mod` 4)) ++ ends (n `div` 4)
generate n = [start !! (n `mod` 4)] ++ ends (n `div` 4)

f x = map fld $ ccl $ generate x
ap x = all isPrime $ f x

main = print $ sum [y | x <- [1..10000], ap x, let y = fld $ generate x, y > 10]