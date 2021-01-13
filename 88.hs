-- Problem 88 : http://projecteuler.net/problem=88 

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
 
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]  
primeFactors n = factors n primes
  where factors n (p:ps) | p*p > n        = [n]
                         | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
                         | otherwise      = factors n ps
isPrime n | n > 1     = (==1) . length . primeFactors $ n
          | otherwise = False
 
facts = concat . takeWhile valid . iterate facts' . (:[])
  where valid xs = length (head xs) > 1
        facts' = nub' . concatMap factsnext
        nub' = S.toList . S.fromList
        factsnext xs = 
          let factsnext' [] = []
              factsnext' (y:ys) = map (form y) ys ++ factsnext' ys
              form a b = a*b : (delete b . delete a $ xs)
          in map sort . factsnext' $ xs        
 
problem_88 =  sum' . extract . scanl addks M.empty . filter (not . isPrime) $ [2..]
  where extract = head . dropWhile (\nm -> M.size nm < 11999)
        sum' = S.fold (+) 0 . S.fromList . M.elems
        addks nm n = foldl (addk n) nm . facts . primeFactors $ n
        addk n nm ps =
          let k = length ps + n - sum ps
              kGood = k > 1 && k < 12001 && k `M.notMember` nm
          in if kGood then M.insert k n nm else nm