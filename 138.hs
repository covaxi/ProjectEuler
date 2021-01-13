-- Problem 138 : http://projecteuler.net/problem=138 
import Data.List

compareTriples (a,b,c) (x, y, z) = compare c z

triples_ 0 = []
triples_ m = sortBy compareTriples ([(a,b,c) | n <- [1..m], let a = m*m-n*n, let b = 2*m*n, let c = m*m+n*n, odd $ m-n, gcd m n == 1] ++ triples_ (m - 1))

triples = [(a,b,c,m,n) | m <- [1..], n <- [1..m], let a = m*m-n*n, let b = 2*m*n, let c = m*m+n*n, odd $ m - n, gcd m n == 1, (a == 2 * b + 1) || (a == 2 * b - 1)]

mn_generator (a, b) = (a*4 + b, a)
-- take 10 $ iterate mn_generator (4, 1)
make_triple (m, n) = (m*m-n*n, 2*m*n, m*m+n*n)
-- take 10 $ map make_triple $ iterate mn_generator (4, 1)

main = print $ sum $ map (\(a,b,c) -> c) $ take 12 $ map make_triple $ iterate mn_generator (4, 1)