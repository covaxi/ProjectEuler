-- Problem 111 : http://projecteuler.net/problem=111 

import Data.List
import Data.Numbers.Primes

n = 10
make0 = [x * 10^(n-1) + t | x <- [1..9], t <- [1,3,7,9]] 
make1 = [p*((10^n - 1) `div` 9 - 10^k) + q*10^k | p <- [1..9], q <- [0..9], k <- [0..n-1], p /= q, (p /= 0 && q /= 0) || k /= n-1] 
make28 = [p * ((10^n - 1) `div` 9 - 10^t - 1) + x * 10^t + y | 
			p <- [2,8], 
			x <-[0..9] \\ [p], 
			y <- [1,3,7,9], 
			t <- [1..n-1], t /= n-1 || x /= 0] 

main = print $ sum $ filter isPrime $ make0 ++ make1 ++ make28

