import Data.Ratio
import Data.Char

-- num :: Integer -> Integer
num y = numerator $ (80110913%90720)*x^9-(1439930609%40320)*x^8+(3676586813%6048)*x^7-(16309308767%2880)*x^6+(135575842079%4320)*x^5-(604813374583%5760)*x^4+(3707549490257%18144)*x^3-(2088457081463%10080)*x^2+(205532761703%2520)*x^1
        where x = toRational y
toString x = map (chr.(+ ord 'a').fromIntegral.(subtract 1)) $ takeWhile (>0) $ map snd $ tail $ iterate ((`divMod` 26).fst) (num x, 0)

main = print $ map toString [0..9]

