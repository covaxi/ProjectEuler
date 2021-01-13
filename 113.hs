-- Problem 113 : http://projecteuler.net/problem=113 

import Data.Array
 
mkArray b f = listArray b $ map f (range b)
 
digits = 100
 
inc = mkArray ((1, 0), (digits, 9)) ninc
dec = mkArray ((1, 0), (digits, 9)) ndec
 
ninc (1, _) = 1
ninc (l, d) = sum [inc ! (l-1, i) | i <- [d..9]]
 
ndec (1, _) = 1
ndec (l, d) = sum [dec ! (l-1, i) | i <- [0..d]]
 
problem_113 = sum [inc ! i | i <- range ((digits, 0), (digits, 9))]
               + sum [dec ! i | i <- range ((1, 1), (digits, 9))]
               - digits*9 -- numbers like 11111 are counted in both inc and dec 
               - 1 -- 0 is included in the increasing numbers