-- Problem 90 : http://projecteuler.net/problem=90 

import Data.List

squares = [('0','2'), ('0','4'), ('0','x'), ('1','x'), ('2','5'), ('3','x'), ('4','x'), ('8','1')]

digits = map ((take 6)) $ permutations "012345x78x"

can (xs,ys) (a,b) = a `elem` xs && b `elem` ys || b `elem` xs && a `elem` ys
