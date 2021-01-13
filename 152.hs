-- Problem 152 : http://projecteuler.net/problem=152 

import Data.Ratio
import Data.List
import Data.Ord (comparing)
import Data.Function (on)
 
invSq n = 1 % (n * n)
sumInvSq = sum . map invSq
 
subsets (x:xs) = let s = subsets xs in s ++ map (x :) s
subsets _      = [[]]
 
primes = 2 : 3 : 7 : [p | p <- [11, 13..79],
                          all (\q -> p `mod` q /= 0) [3, 5, 7]]
 
-- All subsets whose sum of inverse squares,
-- when added to x, does not contain a factor of p
pfree s x p = [(y, t) | t <- subsets s, let y =  x + sumInvSq t,
                        denominator y `mod` p /= 0]
 
 
-- All pairs (x, s) where x is a rational number whose reduced
-- denominator is not divisible by any prime greater than 3;
-- and s is all sets of numbers up to 80 divisible
-- by a prime greater than 3, whose sum of inverse squares is x.
only23 = foldl fun [(0, [[]])] [13, 7, 5]
    where
    fun a p = 
        collect $ [(y, u ++ v) |
        (x, s) <- a,
        (y, v) <- pfree (terms p) x p,
        u <- s]
    terms p = 
        [n * p | 
        n <- [1..80`div`p],
        all (\q -> n `mod` q /= 0) $
        11 : takeWhile (>= p) [13, 7, 5]
        ]
    collect = 
        map (\z -> (fst $ head z, map snd z)) .
        groupBy fstEq . sortBy cmpFst
    fstEq = (==) `on` fst
    cmpFst = comparing fst
 
-- All subsets (of an ordered set) whose sum of inverse squares is x
findInvSq x y = 
    fun x $ zip3 y (map invSq y) (map sumInvSq $ init $ tails y)
    where
    fun 0 _        = [[]]
    fun x ((n, r, s):ns)
        | r > x     = fun x ns
        | s < x     = []
        | otherwise = map (n :) (fun (x - r) ns) ++ fun x ns
    fun _ _        = []
 
-- All numbers up to 80 that are divisible only by the primes
-- 2 and 3 and are not divisible by 32 or 27.
all23 = [n | a <- [0..4], b <- [0..2], let n = 2^a * 3^b, n <= 80]
 
solutions = 
    [sort $ u ++ v |
    (x, s) <- only23,
    u <- findInvSq (1%2 - x) all23,
    v <- s
    ]
 
problem_152 = length solutions