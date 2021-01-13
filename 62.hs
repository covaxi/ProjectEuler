-- Problem 62 : http://projecteuler.net/problem=62 
import Data.List
import Data.Function
import Data.Maybe

cubes = map (^3) [0..9999]
digits = map (sort . show) cubes
main = print $ map (fromJust . (\x -> x `elemIndex` digits) . head) $ filter ((==5) . length) (group (sort digits))
