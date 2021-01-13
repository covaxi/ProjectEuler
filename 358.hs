-- Problem 358 : http://projecteuler.net/problem=358 

import Data.Numbers.Primes

l_max = ceiling (1/0.00000000137)
l_min = floor (1/0.00000000138)
-- 09891
digs = [x | x <- [1..99999], x*56789 `mod` 100000 == 99999]

-- 729809891
-- /2*9 = 3284144505
main = print $ filter (\x -> x `mod` 100000 ==  09891) (takeWhile(<l_max) (dropWhile (<l_min) primes))

