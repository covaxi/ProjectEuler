-- Problem 146 : http://projecteuler.net/problem=146 

-- import Data.Numbers.Primes
import Data.Numbers.Primes
main = let n = 150*10^6 in print $ sum $ filter f [10,20..n] where
  f n = let z = n*n in mod z 3 == 1
    && (let v = mod z 7 in v == 2 || v == 3)
    && mod z 9 /= 0 && mod z 13 /= 0 && mod z 27 /= 0
    && all isPrime [z+1,z+3,z+7,z+9,z+13,z+27]
    && (not $ any isPrime [z+19,z+21])