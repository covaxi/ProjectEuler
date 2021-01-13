numbers = [x*y | x <- [100..999], y <-[x..999]]
digitize x = digitize_ x [] where
		digitize_ x y = if x == 0 then y else digitize_ (x `div` 10) (x `mod` 10 : y)
		
flt (a:b:c:d:e:f:xs) = a == f && b == e && c == d
flt _ = False

quicksort [] = []
quicksort (s:xs) = quicksort [x|x <- xs,x < s] ++ [s] ++ quicksort [x|x <- xs,x >= s]

main = print (quicksort (filter (flt.digitize) numbers))