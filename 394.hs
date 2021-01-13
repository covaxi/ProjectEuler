-- Problem 394 : http://projecteuler.net/problem=394 

f :: (Fractional t, Ord t) => t -> t -> t
f 0 x = 1-(1-1/x)*(1-1/x)

f n x = f0+f1-f0*f1 
    where f1 = f (n-1) x; f0 = f 0 x