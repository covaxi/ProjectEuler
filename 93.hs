-- Problem 93 : http://projecteuler.net/problem=93 

import Data.Ratio
import Data.List
import Data.Ord

plus :: Integral a => [Ratio a] -> Ratio a -> [Ratio a]
[] `plus` a = [a,-a]
(b:xs) `plus` a = [a * b, a + b, a - b, b - a] ++ if b == 0 then [] else [a / b] ++ if a == 0 then [] else [b / a] ++ (xs `plus` a)

allNumbers x = nub $ sort $ map numerator $ filter (isNatural) $ concatMap (foldl plus []) $ permutations x

isNatural x = x > 0 && denominator x == 1 

upTo x = length $ takeWhile (id) $ zipWith (==) [1..] $ allNumbers x

main = print $ maximumBy (comparing snd) [(x,y) | a <- [1..6], b <- [a+1..7], c <- [b+1..8], d <- [c+1..9], let x = ((a*10+b)*10+c)*10+d, let y = upTo [a,b,c,d]]