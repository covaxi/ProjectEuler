-- Problem 356 : http://projecteuler.net/problem=356 

f :: (Integral a) => a -> Double -> Double
f n e = -e^3 + 2^(n+1) * e^2 - 2^(2*n) * e + fromIntegral n
f' n e = -3*e^2 + 2^(n+2) * e - 2^(2*n)
newt n x = x - f n x / f' n x
solve n = iterate (newt n) 0 !! 100
fracs = map solve [1..30]
x0 = take 30 (iterate (*2) 2)

(x-2^n)xx+n
(2^n-x)xx = n
ln*2^n-x + 2 ln x = ln n