divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2] ++ [n]

triangles = [x * (x+1) `div` 2 | x <- [1..]]


multipliers = [if x `mod` 2 == 0 then x `div` 2 else x | x <- [1..]]
factors = map (length . divisors) multipliers
dividers = [ a * b | (a,b) <- zip factors $ tail factors]

main = print $ head $ dropWhile (\(a,b) -> b <= 500) $ zip triangles dividers