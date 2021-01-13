-- Problem 139 : http://projecteuler.net/problem=139 

ps = 12 : 70 : zipWith (\x y -> 6 * x - y) (tail ps) ps

test l = sum . takeWhile (> 0) $ map (div (l - 1)) ps

main = print $ test (10 ^ 8)