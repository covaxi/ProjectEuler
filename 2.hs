fibs = 0:1:[a+b|(a,b)<-zip fibs $ tail fibs]
evens = filter (even) fibs
main = print $ sum $ takeWhile (<4000000) evens