-- Problem 169 : http://projecteuler.net/problem=169 

f = fst . f'

f' 0 = (1,0)
f' n
 | even n = (a+b,b)
 | odd  n = (a,a+b)
 where (a,b) = f' (n `div` 2)