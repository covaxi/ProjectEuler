-- Problem 121 : http://projecteuler.net/problem=121 

import Data.List
import Data.Ratio
import Data.Digits

n = 15

fixLength x = x ++ replicate (n-len) 0
    where len = length x

test x = sum x > (toInteger $ length x) `div` 2

probs = take n [1 % x | x <- [2..]]
results = map (fixLength . digitsRev 2) [0..2^n-1]
wins = filter test results
total = sum $ map (product.filter(/=0).zipWith (\a b -> if b == 0 then 1-a else a) probs.map(toRational)) wins
result = numerator $ head [x | x <- [1..], x * total > 1] - 1
main = print result