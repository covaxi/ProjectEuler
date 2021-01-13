-- Problem 361 : http://projecteuler.net/problem=361 
import Data.Array
import Data.List

toBinary x = concatMap (show) $ reverse $ map snd $ takeWhile (/= (0,0)) $ tail $ iterate ((`divMod` 2).fst) (x , 0)

n = 200
tueLen' = array (1, n) ([(1,2), (2,2), (3,3), (4,5)] ++ [(x, 2*la + ls) | x <- [5..n], let la = tueLen' ! (x - 3), let ls = s10Len' ! (x - 3)])
s10Len' = array (1, n) ([(1,0), (2,1), (3,2), (4,4)] ++ [(x, la + ls) | x <- [5..n], let la = tueLen' ! (x - 3), let ls = s10Len' ! (x - 3)])

tue 1 x = [0, 1] !! x
tue 2 x = [2, 3] !! x
tue 3 x = [4, 5, 6] !! x
tue 4 x = [9, 10, 11, 12, 13] !! x
tue y x
        | x < la = 2^(y-1) + tue (y-3) x
        | x < la + ls = 2^(y-1)+2^(y-3) + tue (y-3) (x-la)
        | otherwise = 2^(y-1)+2^(y-2) + tue (y-3) (x-la-ls)
        where 
            la = tueLen' ! (y - 3)
            ls = s10Len' ! (y - 3)

find' y x
        | x < ln = tue y x
        | otherwise = find' (y+1) (x-ln)
        where ln = tueLen'! y

find x = find' 1 x

tt 0 = [0]
tt x = tt (x-1) ++ (map (1-) $ tt $ x - 1)

fnd _ [] = []
fnd x (y:ys)
	| y == 0 = fnd x ys
	| length ys > x = sort $ nub $ (y : (take (x-1) ys)) : fnd x ys
	| otherwise = []


