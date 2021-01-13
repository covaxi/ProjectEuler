-- Problem 57 : http://projecteuler.net/problem=57 

x0 = (1,1)

f (a,b) = (x `div` d, y `div` d) where
            x = a + b + b
            y = a + b
            d = gcd x y
            
check (a, b) = (length $ show a) > (length $ show b)

main = print $ length $ filter check $ take 1000 (iterate f x0)
