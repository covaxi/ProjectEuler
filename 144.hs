-- Problem 144 : http://projecteuler.net/problem=144 

solveQuad = (\(p,q)->map ($sqrt(p*p/4-q)) [(-p/2+),(-p/2-)])

reflect=(\(vx,vy) (mx,my) -> let k=2*(vx*mx+vy*my)/(mx*mx+my*my)in(k*mx-vx,k*my-vy))

nextPoint = (\((x0,y0),(vx,vy))->let u1@(x1,y1)=(\t->(x0+t*vx,y0+t*vy))$head$solveQuad$(\(a,b,c)->(b/a,c/a))(4*vx*vx+vy*vy, 8*x0*vx+2*y0*vy,4*x0*x0+y0*y0-100) in (u1, reflect (vx,vy) (y1,-4*x1)))

main = print $ length$takeWhile (not.(\(x,y)->y>=0 && -0.01<=x && x<=0.01).fst)$drop 1 $ iterate nextPoint ((0,10.1), (1.4-0,-9.6-10.1))