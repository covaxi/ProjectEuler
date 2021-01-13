main = print $ sum $ map digits [1..1000] where
    digits x 
        | x == 0  = 0
        | x == 1 || x == 2 || x == 6 || x == 10 = 3
        | x == 4 || x == 5 || x == 9 = 4
        | x == 3 || x == 7 || x == 8 = 5
        | x == 11 || x == 12 = 6
        | x == 13 || x == 14 || x == 18 || x == 19 = 8
        | x == 15 || x == 16 = 7
        | x == 17 = 9
        | x < 40 = 6 + digits y
        | x < 70 = 5 + digits y
        | x < 80 = 7 + digits y
        | x < 100 = 6 + digits y
        | x < 1000 = (digits $ x `div` 100) + 7 + (if x `mod` 100 == 0 then 0 else 3) + digits (x `mod` 100)
        | x == 1000 = 8 + 3
        | otherwise = 0
        where 
            y = x `mod` 10
