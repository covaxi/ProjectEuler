-- Problem 68 : http://projecteuler.net/problem=68 

import Data.List

isOk [a,b,c,d,e,f,g,h,i,j] =
  s == d + c + e &&
  s == f + e + g && 
  s == h + g + i &&
  s == j + i + b where s = a + b + c
 
l16 [a,b,c,d,e,f,g,h,i,j] = a < d && a < f && a < h && a < j && (d == 10 || f == 10 || h == 10 || j == 10)

ss [a,b,c,d,e,f,g,h,i,j] =
        show a ++ show b ++ show c
    ++  show d ++ show c ++ show e
    ++  show f ++ show e ++ show g
    ++  show h ++ show g ++ show i
    ++  show j ++ show i ++ show b

main = print $ maximum $ map ss $ filter (isOk) $ filter (l16) $ permutations [1..10]