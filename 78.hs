-- Problem 78 : http://projecteuler.net/problem=78 

import Data.Maybe
import Data.Array
import Data.List


p = array (0, mx) $ (0,1) : [(n, sum $ zipWith (*) (cycle [1,1,-1,-1]) (map (\t -> p!(n-t)) $ takeWhile (<=n) pentas)) | n <- [1..mx]]
    where
        pentas = map penta $ [a*b | a <- [1..], b <- [1,-1]]
        penta x = (3 * x * x - x) `div` 2
        mx = 100000
main = print $ 1 + fromJust ( findIndex ((==0) . (`mod` 1000000)) $ map (p!) [1..])
        
