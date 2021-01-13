-- Problem 151 : http://projecteuler.net/problem=151 

import Data.List
import Data.Maybe
import Data.Ratio

data Paper = A2 | A3 | A4 | A5 deriving (Eq,Show)
new_week = [ A2, A3, A4, A5 ]

-- Generates a new contents of the envelope provided that we've taken
-- a `choice' sheet out of envelope which contains `sheets'
take_sheet sheets choice = delete choice sheets ++ leftovers choice
leftovers A2 = [A3,A4,A5]
leftovers A3 = [A4,A5]
leftovers A4 = [A5]
leftovers A5 = []

-- Gets a description of a step in the markov chain in the form of
-- (probability with which we got here, number of times we got 1 sheet in envelope, current set of sheets)
-- and produces all possible continuations from this step, along with updated probability and count
gen_next [] = Nothing
gen_next ((p,c,[A5]):rest) = Just ( (p,c,[A5]), rest )
gen_next ((p,c,sheets):rest) =
  let c' = if length sheets == 1 then c+1 else c
      next_step = catMaybes $ map (choose c' sheets) [A2,A3,A4,A5]
      in Just ( (p,c',sheets) , (next_step ++ rest) )
  where
  choose c sheets choice = 
    let probability = (fromIntegral $ length (filter (==choice) sheets))%(fromIntegral $ length sheets)
        in case probability of
                0 ->  Nothing
                p' -> Just (p*p', c, take_sheet sheets choice)

-- Generate all possible flows for 1 week, and compute expected value
problem151 =
  sum $ map (\(p,c,_)-> p*c) $ filter (\(p,c,s) -> (s==[A5])) $ unfoldr gen_next [(1,0,new_week)]