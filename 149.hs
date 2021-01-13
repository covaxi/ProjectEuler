-- Problem 149 : http://projecteuler.net/problem=149 

import Data.Array
import Data.List (foldl')

n = 2000

res = maximum' $ concat [rows, cols, diags, diags']
    where
        rows   = map (maxSumInRow . getRow   laggedFibArray) [0 .. n-1]
        cols   = map (maxSumInRow . getCol   laggedFibArray) [0 .. n-1]
        diags  = map (maxSumInRow . getDiag  laggedFibArray) [-(n-2) .. (n-2)]
        diags' = map (maxSumInRow . getDiag' laggedFibArray) [-(n-2) .. (n-2)]


laggedFibArray :: Array Integer Integer
laggedFibArray = listArray (0, n^2-1) $ map f [1..n^2]
    where
        f k = norm $ if k < 56
              then 100003 - (200003*k) + (300007*(k^3))
              else (laggedFibArray ! (k-25)) + (laggedFibArray ! (k-56)) + (10^6)

        norm x = mod x (10^6) - 500000


getRow   a i = map (a!) [i*n .. (i+1)*n-1]
getCol   a i = map (a!) [i,n+i .. n*(n-1)+i]
getDiag  a i = map (a!) $
    if i >= 0
    then [(i*n) + (k*(n+1)) | k <- [0..n-i-1]]
    else [k + n*(k+i) | k <- [-i .. n-1]]
getDiag' a i = map (a!) $
    if i >= 0
    then [(n*k) + n-k-i-1 | k <- [0..n-i-1]]
    else [n*(k-i) + n-k-1 | k <- [0..n+i-1]]


maxSumInRow = snd . foldl' f (0,0)
    where
        f (line_sum, line_max) x = (line_sum', max line_max line_sum')
            where line_sum' = max (line_sum+x) 0


{- old version that is very stupid
maxSumInRow row = maxSumInRow' (maximum' sum1) sum1 row
    where
        sum1 = sumsInRow row $ repeat 0

        sumsInRow row prev = zipWith3 (\a b c -> a+b-c) row (tail row) (tail prev)

        maxSumInRow' m row prev = case row of
            [m'] -> max m m'
            _    -> maxSumInRow' (max m $ maximum' sums) sums row
            where sums = sumsInRow row prev
-}


-- strict version of maximum
maximum' (x:xs) = foldl' max x xs