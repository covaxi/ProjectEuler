-- Problem 147 : http://projecteuler.net/problem=147 

rect n m = 
  let u = div (n*(n+1)*(n+2)*m*(m+1)*(m+2)) 36
      v = sum [(6*k-7)*(i-k+1)*(j-k+1) | i<-[2..n], j<-[2..m], k<-[2..min i j]]
      w = sum [k*((i-k+1)*(j-k)+(i-k)*(j-k+1)) | i<-[1..n], j<-[1..m], k<-[1..min i j]]
  in u+v+w

main = print $ rect 47 43