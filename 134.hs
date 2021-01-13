-- Problem 134 : http://projecteuler.net/problem=134 

import Data.Digits (digits)
import Data.Numbers.Primes (primes)

findPrimePairNumber :: (Int, Int) -> Int
findPrimePairNumber (p1, p2) = k * p2
  where
    m = 10 ^ (length . digits 10 $ p1)
    (a, _) = extendedEuclidean p2 m
    k = p1 * a `mod` m

extendedEuclidean :: Int -> Int -> (Int, Int)
extendedEuclidean _ 0 = (1, 0)
extendedEuclidean 0 _ = (0, 1)
extendedEuclidean a b = let (x, y) = extendedEuclidean b (a `rem` b)
                        in (y, x - (a `quot` b) * y)
    
main :: IO ()
main = print . sum $ map findPrimePairNumber pps
  where
    pps = takeWhile ((<= 1000000) . fst) .
          dropWhile ((< 5) . fst) $
          primes `zip` (tail primes)