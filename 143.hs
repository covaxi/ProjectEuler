-- Problem 143 : http://projecteuler.net/problem=143 

import Control.Monad
import Data.List
import qualified Data.Map as M
import qualified Data.IntSet as S

eisenstein :: Int -> [(Int,Int)]
eisenstein upp = do n <- [1..sqrt' upp -1]
                    m <- [n+1..min (div (upp-n*n) (2*n)).sqrt'$ upp + n^2]
                    guard$ mod (m-n) 3 /= 0
                    guard$ gcd n m == 1
                    let a = (m+n)*(m-n)
                        b = n*(2*m+n)
                    k <- [1..div upp $ a+b]
                    return (k*a,k*b)
    where sqrt' = floor.sqrt.fromIntegral

torricelli :: M.Map Int S.IntSet -> Int -> [Int]
torricelli eisen p = case M.lookup p eisen of
                       Nothing -> []
                       Just qs -> do q <- S.toList qs
                                     r <- findR (S.toList qs) q
                                     return$ p+q+r
    where findR qs q = case M.lookup q eisen of
                         Nothing -> []
                         Just rs -> S.toList. S.intersection rs . S.fromList $ dropWhile(<=q) qs

p143 n = nub.takeWhile(<n).sort.concatMap (torricelli e)$ [1..div n 3]
    where e = foldl insert' M.empty .eisenstein$  n
          insert' mp (a,b) = let [p,q] = sort [a,b]
                             in M.insertWith f p (S.singleton q) mp
          f new old = let [n] = S.toList new
                      in S.insert n old

main = print.sum.p143 $ 120000 