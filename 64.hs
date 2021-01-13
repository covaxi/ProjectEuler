-- Problem 64 : http://projecteuler.net/problem=64 
import Debug.Trace

sq x = last $ takeWhile (\t -> t*t <= x) [0..]

x0 x = (sq x, sq x, 1)

f x (_,a,b) = (c,d,q) where
    qq = x - a*a
    q = qq `div` b
    c = (a + sq x) `div` q
    d = q*c - a
    
 
fst' (a,_,_) = a
x1 x = f x $ x0 x
x2 x = f x $ x1 x
chain x = (x0 x):(x1 x):(takeWhile (/= (x1 x)) (iterate (f x) $ x2 x))
isOk x = ((sq x)^2 /= x) && (even $ length $ chain x)
main = print $ length $ filter (isOk) [1..10000]
