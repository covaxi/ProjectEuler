-- Problem 140 : http://projecteuler.net/problem=140 

import Data.List

result = (sum . take 30 . nub . filter (> 0) . 
                map fst . concat . transpose . 
                map (iterate f)) p0s where
    f (x, y) = (-9 * x - 4 * y - 14, -20 * x - 9 * y - 28)
    p0s = [ (0, -1), (0, 1), (-3, -2), (-3, 2), 
            (-4, -5), (-4, 5), (2, -7), (2, 7) ]