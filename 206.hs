-- Problem 206 : http://projecteuler.net/problem=206 

isqrt :: Integer -> Int
isqrt = floor . sqrt . fromIntegral

from = 10203040506070809
to = 19293949596979899
start = isqrt from
stop = isqrt to

all' = [z | z <- [start .. stop], let x = z*z,
             let y9 = x `mod` 10, y9 == 9,
             let y8 = x `mod` 1000, y8 >= 809 && y8 <= 899,
             let y7 = x `mod` 100000, y7 >= 70000 && y7 <= 79899,
             let y6 = x `mod` 10000000, y6 >= 6000000 && y6 <= 6979899,
             let y5 = x `mod` 1000000000, y5 >= 500000000 && y5 <= 596979899,
             let y4 = x `mod` 100000000000, y4 >= 40000000000 && y4 <= 49596979899,
             let y3 = x `mod` 10000000000000, y3 >= 3000000000000 && y3 <= 3949596979899,
             let y2 = x `mod` 1000000000000000, y2 >= 200000000000000 && y2 <= 293949596979899]

main = do putStrLn $ show ((head all') * 10)