-- Problem 98 : http://projecteuler.net/problem=98 

module Main where
import Data.List
import Control.Monad
import Data.Maybe

permute ns xs ys = map (\x -> fromJust (lookup x table)) xs
 where
 table :: [(Char, Char)]
 table = zip ys ns

cmp a b = sort a `compare` sort b
cmp' a b = sort a == sort b
main = do
    f <- readFile "words.txt"
    let ws = read f :: [String]
    let anagrams = (filter ((>1) . length) $ groupBy cmp' $ sortBy cmp ws) >>= setsOf
    print $ sort (anagrams >>= solve)
    getLine
    
solve [a,b] = (ss >>= check a b)
 where n = length a
       l = 10 ^ (n-1)
       h = 10 ^ n
       ss = takeWhile (<h) $ dropWhile (<l) $ map (^2) [1..]
       
check a b n = do
 guard (isSquare rn)
 guard (head n' /= '0')
 guard (nub n' == n')
 [(rn,a,b)]
 where ns = show n
       n' = permute ns a b
       rn = read n'   
isSquare x = floor r == ceiling r where r = sqrt (fromIntegral x)  
setsOf xs = [ [a,b] | a <- xs, b <- xs, a /= b ]
