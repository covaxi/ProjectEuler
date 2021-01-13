-- Problem 128 : http://projecteuler.net/problem=128 

import Data.Numbers.Primes

p=  concat [m |a <- [0..70000],let m=middle a++right a,not$null m]
    where
    middle n
        |all isPrime [11+6*n,13+6*n,29+12*n]=[2+3*(n+1)*(n+2)]
        |otherwise=[]
    right n
        |all isPrime [11+6*n,17+6*n,17+12*n]=[1+3*(n+2)*(n+3)]
        |otherwise=[]
main = print(p!!1997)
