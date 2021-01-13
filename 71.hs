-- Problem 71 : http://projecteuler.net/problem=71 

main = print $ maximum $ filter (< 3 % 7) $ map (\x -> 3 * x `div` 7 % x) [1..10^6]
