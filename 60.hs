-- Problem 60 : http://projecteuler.net/problem=60 

import Data.Numbers.Primes
import Debug.Trace

n = 4000
zad x y = read (show x ++ show y)
primes' = take n $ primes
main = print $ [(x,y,z,t,q,x+y+z+t+q)  |
    x <- primes', x /= 5, x > 5,
    let yp = getPrimes (trace ("testing x: " ++ show x) x) primes',
    y <- yp,
    let zp = getPrimes y yp,
    z <- zp,
    let tp = getPrimes z zp,
    t <- tp,
    q <- getPrimes t tp] where getPrimes m xs = filter (\a -> (a > m) && all isPrime [zad a m, zad m a]) xs

-- (3,3119,9887,36263,48731,98003) is too big, decreasing n from 10000 to 5000:)
-- (3,5323,10357,29587,31231,76501)
-- (7,1237,2341,12409,18433,34427) changing n to 4000
-- (13,5197,5701,6733,8389,26033)