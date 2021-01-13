-- Problem 48 : http://projecteuler.net/problem=48 

main = print $ reverse $ take 10 $ reverse $ show $ foldl1 (+) [x^x | x <- [1..1000]]