-- Problem 25 : http://projecteuler.net/problem=25 

fibs = 0:1:[a+b|(a,b)<-zip fibs $ tail fibs]
main = print $ head $ dropWhile (\(a,b) -> a < 10^999) $ zip fibs [0..]
