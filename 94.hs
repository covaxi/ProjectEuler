-- Problem 94 : http://projecteuler.net/problem=94 


-- much faster N ~= M * sqrt(3) && M*(2+sqrt(3))
triangles = [(a,b,c,m,n,m/n) | m <- [1..13000], n <- [1..m], odd $ m - n, gcd m n == 1, let a = m*m-n*n, let b = 2*m*n, b <= 333333333, let c = m*m+n*n, c <= 333333334, or [c == 2*b+1, c == 2*b-1, c == 2*a-1, c == 2*a+1]]
main = print $ sum $ map (\(a,b,c) -> if (c == 2*a-1 || c == 2*a+1) then 4*a+c else 4*b+c) triangles