s x = if x < 10 then x else s (x `div` 10) + (x `mod` 10)
main = print $ s (product [1..100])
