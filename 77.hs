-- Problem 77 : http://projecteuler.net/problem=77 

import Data.Numbers.Primes
import Data.List

f a b = with where 
                (poor,rich) = splitAt b a
                with = poor ++ zipWith (+) with rich

                
counter = foldl (f) (1 : repeat 0)
ways = counter $ take 100 primes
 
main =  print $ find ((>5000) . (ways !!)) $ [1..]

    