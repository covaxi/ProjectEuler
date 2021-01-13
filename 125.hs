import Data.List as L
import Data.Set as S
 
hi = 100000000
 
ispalindrome n = (show n) == reverse (show n)
 
-- the "drop 2" ensures all sums use at least two terms
-- by ignoring the 0- and 1-term "sums"
sumsFrom i =
    takeWhile (<hi) .
    drop 2 .
    scanl (\s n -> s + n^2) 0 $ [i..]
 
limit =
    truncate . sqrt . fromIntegral $ (hi `div` 2)
 
main =
    print .
    fold (+) 0 .
    fromList .
    concat .
    L.map (L.filter ispalindrome . sumsFrom) $ [1 .. limit]
