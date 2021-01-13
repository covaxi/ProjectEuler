import Control.Monad.ST
import Control.Monad
import Data.Array.MArray
import Data.Array.ST
import Data.List
import Data.Map (fromList,(!))
import Text.Regex 
import Data.Ord (comparing)
makeArr x=map zero (splitRegex (mkRegex ",") x)  
makeNet x lst y=[((a,b),m)|a<-[0..x-1],b<-[0..a-1],let m=lst!!a!!b,m/=y]
zero x
    |'-' `elem` x=0
    |otherwise=read x::Int
problem_107 =do
    a<-readFile "network.txt"
    let b=map makeArr $lines a
        network = makeNet 40 b 0
        edges = sortBy (comparing snd) network 
        eedges =map fst edges
        mape=fromList edges
        d=sum $ map snd edges 
        e=sum$map (mape!)$kruskal eedges
    print (d-e)
kruskal es = runST ( do
    let hi = maximum $ map (uncurry max) es
        lo = minimum $ map (uncurry min) es
    djs <- makeDjs (lo,hi)
    filterM (kruskalST djs) es)
 
kruskalST djs (u,v) = do
    disjoint <- djsDisjoint u v djs
    when disjoint $ djsUnion u v djs
    return disjoint
 
type DisjointSet s = STArray s Int (Maybe Int)
 
makeDjs :: (Int,Int) -> ST s (DisjointSet s)
makeDjs b = newArray b Nothing
 
djsUnion a b djs = do
    root <- djsFind a djs
    writeArray djs root $ Just b
 
djsFind a djs = maybe (return a) f =<< readArray djs a
 where f p = do p' <- djsFind p djs
                writeArray djs a (Just p')
                return p'
 
djsDisjoint  a b uf = liftM2 (/=) (djsFind a uf) (djsFind b uf)