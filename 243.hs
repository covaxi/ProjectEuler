-- Problem 243 : http://projecteuler.net/problem=243 

import Data.Function
import Data.Numbers.Primes
import Data.List
import Data.Ratio

eulerTotient n = numerator $ n % 1 * product [y | x <- nub $ primeFactors n, let y = 1 - 1 % x]

n = 15499 % 94744

t x = eulerTotient x % (x-1)
tt x = t $ product $ take x primes

-- Return the combinations, with replacement, of k items from the
-- list.  We ignore the case where k is greater than the length of 
-- the list.
combsWithRep 0  _ = [[]]
combsWithRep _ [] = []
combsWithRep k xxs@(x:xs) = map (x:) (combsWithRep (k-1) xxs) ++ combsWithRep k xs
 
binomial n m = (f n) `div` (f (n - m)) `div` (f m) where
	f n = if n == 0 then 1 else n * f (n - 1)
 
countCombsWithRep k lst = binomial (k - 1 + length lst) k
-- countCombsWithRep k = length . combsWithRep k

a = head $ sortBy (compare `on` denominator) $ filter (< n) $ map t $ filter (< (product $ take 10 primes)) $ map product $ combsWithRep 11 $ take 14 primes
main = print $ 1 + (denominator a)