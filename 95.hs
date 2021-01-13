-- Problem 95 : http://projecteuler.net/problem=95 

import Data.Numbers.Primes
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Array


sumDivisors x = (product $ map fn c) - x
    where
        fn (a,b) = (a ^ (b + 1) - 1) `div` (a - 1)
        c = zip a b 
        a = nub pf
        b = map length $ group pf
        pf = primeFactors x
        
       
amicables = array (1, 999999) [(x, chain x) | x <- [1..999999]] where 
    chain x = chain' x [] where
        chain' x' ys
            | sd == 0 = []
            | sd == x' = [x']
            | sd > 999999 = []
            | sd `elem` ys = x':ys
            | sd < x = amicables ! sd
            | otherwise = chain' sd (x':ys)
           where sd = sumDivisors x'
           
    
        
        
-- 14316 is the answer

-- > elemIndex (maximum $ map length $ elems amicables) (map length $ elems amicables)
-- Just 5915
-- (63.52 secs, 47718960704 bytes)
-- > amicables ! 5915
-- []
-- (0.00 secs, 0 bytes)
-- > amicables ! 5916
-- [17716,19916,22744,22976,45946,48976,97946,122410,152990,285778,381028,376736,243760,275444,274924,366556,418904,358336,294896,589786,629072,295488,177792,83328,47616,31704,19116,14316,9204,5916]
-- (0.00 secs, 528268 bytes)
-- > length $ amicables ! 5916
-- 30
-- (0.00 secs, 519672 bytes)
-- > minimum $ amicables ! 5916
-- 5916
-- (0.00 secs, 0 bytes)
-- > sumDivisors 5916
-- 9204
-- (0.00 secs, 521900 bytes)
-- > sumDivisors 17716
-- 14316
-- (0.00 secs, 517676 bytes)
-- > sumDivisors 14316
-- 19116
-- (0.00 secs, 517676 bytes)
-- > sumDivisors 19116
-- 31704
-- (0.00 secs, 517636 bytes)
-- > sumDivisors 17716
-- 14316
-- (0.00 secs, 517656 bytes)
-- > sumDivisors 19916
-- 17716
-- (0.00 secs, 517636 bytes)