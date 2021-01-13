-- Problem 80 : http://projecteuler.net/problem=80 

import Data.Char

sqr a = head $ drop 1000 $ iterate (f a) 1
    where f a x = (x + a `div` x) `div` 2
    
makeMoreDigits x = x*10^210

first100 x = sum $ map digitToInt $ take 100 $ show $ sqr $makeMoreDigits x

main = print $  sum $ filter (>10) $ map first100 [1..100]