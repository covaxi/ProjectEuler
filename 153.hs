-- Problem 153 : http://projecteuler.net/problem=153 
import Data.List
import Debug.Trace

data Cond a = a :? a
 
infixl 0 ?
infixl 1 :?
 
(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y


sqrtInt :: Integer -> Integer
sqrtInt = floor . sqrt . fromIntegral

n :: Integer
n = 100000000
sn :: Integer
sn = sqrtInt n
sn2 :: Integer
sn2 = sqrtInt (n`div`2)

sum_divisors :: Integer -> Integer
sum_divisors n = sum [n`div`a*a | a <- [1..n]]
sum_equals :: Integer -> Integer
sum_equals n = sum [n`div`(2*a)*2*a | a <- [1..n`div`2]]
sum_greater :: Integer -> Integer
sum_greater n = sum [n`div`(i*ab)*2*i*(a+b) | a <- [1..sn2], b <- [a+1..sn], 1 == gcd a b, let ab = a*a+b*b, i <- [1..n`div`ab]]

main = putStrLn $ show $ sum [sum_greater n, sum_equals n, sum_divisors n]