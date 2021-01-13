-- Problem 35 : http://projecteuler.net/problem=35 

union (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys 
           GT -> y : union (x:xs)  ys
		   
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
		
		

		
		

shft y x = x `div` 10 + ((10^(y-1)) * (x `mod` 10))
ccc x y = iterate (shft y) x
cc x y = h : takeWhile (/= h) t where (h:t) = ccc x y
allPrime (y,x) = all isPrime (cc x y)
alln 0 = []
alln n = alln (n-1) ++ zip (repeat n) [(10^n-1) `div` 9..(10^n-1)] 
main = print $ length $ filter (allPrime) $ alln 6
