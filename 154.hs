-- Problem 154 : http://projecteuler.net/problem=154 
-- 479742450

import Debug.Trace
import Data.Numbers.Primes
import Data.Array


n = 200000
m = 12
num_zeroes x = div x 5

makearray k = array (0,n) (concat [[(a+b, p) | a <- [0..k-1], b+a <= n ]| b <- [0,k..n], 
    let p = sum $ map (b `div`) $ takeWhile(<=b) $ iterate (*k) k])

twos = makearray 2
fives = makearray 5
tens x = (twos!x, fives!x)
fiven = fives ! n

test x y | x `mod` y == 0 = trace (show x) True
test x _ = True

k = 5000

-- even n only
s = [(n,n-a-b, a,b) | a <- [0..n `div` 2], test a 10,
    b <- [0..(n-a-1)`div`2], 
    fiven - fives!(n-a-b)-fives!a-fives!b < m]

f a b = product [n-a-b+1..n] `div` product [1..a] `div` product [1..b]
g a b = fives!n - fives!(n-a-b) - fives!(a) -fives!(b)
d xx = filter (\x -> g x xx > 11) [1..n-xx]

gg a b = twos!n - twos!(n-a-b) - twos!(a) - twos!(b)

dd xx | xx `mod` 10 == 0 && trace (show xx) False = undefined
dd xx = filter (\x -> gg x xx > 11 && g x xx > 11) [1..n-xx]

main = putStrLn $ show $ sum $ map (length.dd) [0..n]

