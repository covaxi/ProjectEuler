-- Problem 148 : http://projecteuler.net/problem=148 

triangel 0 = 0
triangel n 
    |n <7 =n+triangel (n-1)  
    |n==k7 =28^k 
    |otherwise=(triangel i) + j*(triangel (n-i))
    where
    i=k7*((n-1)`div`k7)
    j= -(n`div`(-k7))
    k7=7^k
    k=floor . logBase 7 . fromIntegral $ n
main = print $ triangel (10^9)