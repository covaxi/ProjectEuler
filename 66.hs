-- Problem 66 : http://projecteuler.net/problem=66 

import Data.Numbers.Primes
import Data.List
import Data.Maybe
import Data.Ord
import Data.Ratio
import Data.Function

sq x = last $ takeWhile (\t -> t*t <= x) [0..]

x0 x = (sq x, sq x, 1)

f x (_,a,b) = (c,d,q) where
    qq = x - a*a
    q = qq `div` b
    c = (a + sq x) `div` q
    d = q*c - a
    
 
fst' (a,_,_) = a
chain x = map (\(a,b,c) -> a) $ iterate (f x )(x0 x)
ff x y = 1 / y + x
g b = numerator $ fromJust $ find (\x -> ((numerator x)^2 - b*(denominator x)^2) == 1) (map (\a -> (foldr1 ff (map (\x -> x%1) $ take a $ chain b))) [1..])
main = print $ maximumBy (comparing snd) [(x, g x) | x <- [2..1000], ((sq x) ^ 2) /= x]

