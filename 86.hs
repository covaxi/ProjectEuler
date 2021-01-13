-- Problem 86 : http://projecteuler.net/problem=86

import           Data.List

sqr m = round $ sqrt $ fromIntegral m
isSquare m = m == sqr m ^ 2
cube m = sum [(a`div`2) - if a > m then a - m - 1 else 0 | a <- [1..2*m], isSquare (a*a + m*m)]
triangles t = filter (\(a,b) -> a <= t && b <= 2*t) [(k * min a b, k * max a b) | m <- [1..2 * sqr t], n <- [1..m], odd(m-n), gcd m n == 1, let a = m*m-n*n, let b = 2*m*n, let c = m*m+n*n, k <- [1..t `div` (max a b)]]

n m (a,b) = (if 2*a > b then 2*a-b+1 else 0) + (if m >= b then length [1..a-1] else 0)
tri t = map (n t) $ triangles t

main = print $ findIndex (>1000000) (scanl (+) 0 (map cube [1..]))
