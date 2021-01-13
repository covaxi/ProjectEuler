-- Problem 61 : http://projecteuler.net/problem=61 

(a,b,c) `nxt` (d,e,f) = c `div` 100 == f `mod` 100
p x = takeWhile (\(x,y) -> y < 10000) $ dropWhile (\(x,y) -> y < 1000) $ zip [1..] (scanl1 (+) [1,x-1..])
pp = concatMap (\x -> map (\(a,b) -> (x,a,b)) $ p x) [3..8]
main = print [ sum $ pechatat [x1,x2,x3,x4,x5,x6] |
    x1 <- pp,
    let p2 = getNext x1 pp, x2 <- p2, x2 `nxt` x1,
    let p3 = getNext x2 p2, x3 <- p3, x3 `nxt` x2,
    let p4 = getNext x3 p3, x4 <- p4, x4 `nxt` x3,
    let p5 = getNext x4 p4, x5 <- p5, x5 `nxt` x4,
    let p6 = getNext x5 p5, x6 <- p6, x6 `nxt` x5, x1 `nxt` x6
    ] where
        getNext x@(x1,x2,_) xs = filter (\(a,b,c)-> a /= x1 && b /= x2) xs
        pechatat = map (\(a,b,c) -> c)