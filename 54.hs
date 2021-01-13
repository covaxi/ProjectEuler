-- -- Problem 54 : http://projecteuler.net/problem=54 

-- import Data.List
-- import Maybe
-- import Data.Function
-- import Data.Ord

-- cards = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']
-- suits = ['C', 'H', 'S', 'D']

-- readCard :: String -> (Int,Int)
-- readCard (c:s:[]) = ((fromJust $ elemIndex c cards) + 2, fromJust $ elemIndex s suits)

-- readHands line = let cards = map readCard $ words line 
                 -- in (take 5 cards, drop 5 cards)


-- flush x = (length $ nubBy ((==) `on` snd) x) == 1

-- straight x = straight' $ sort $ map fst x
-- straight' (x:y:xs) = ((y-x) == 1) && (straight' $ y:xs)
-- straight' _ = True

-- quality x = q ++ [fst $ head $ head q1] ++ q2 where 
          -- q1 = reverse $ sortBy (compare `on` length) $ groupBy ((==) `on` fst) x
          -- q | straight x && flush x = [5]
            -- | length q1 == 2 = q3
            -- | flush x = [3,1,3]
            -- | straight x = [3,1,2]
            -- | otherwise = q3
          -- q3 = map length q1
          -- q2 = reverse $ sort $ map (fst) x



					
-- printMe [] = return ()
-- printMe (x:xs) = do
	-- putStrLn x
	-- printMe xs
					
-- main = do raw <- readFile "54.txt"
          -- let hands = map readHands $ lines raw
          -- let result = filter (\x -> fst x > snd x) hands
          -- print $ length result
		  
		  
module Main where 

import Data.List
import Data.Maybe
import Data.Function

testSet = map (\(line, winner) -> (readHands line, winner))
        [("5H 5C 6S 7S KD 2C 3S 8S 8D TD", False)
        ,("5H 5C 6S 7S KD 2C 3S 4S 5D 6D", False)
        ,("5D 8C 9S JS AC 2C 5C 7D 8S QH", True)
        ,("2D 9C AS AH AC 3D 6D 7D TD QD", False)
        ,("4D 6S 9H QH QC 3D 6D 7H QD QS", True)
        ,("2H 2D 4C 4D 4S 3C 3D 3S 9S 9D", True)
        ,("AH KD QH JD 3H AH KH QH JD 2H", True)
        ,("3H 4D 5H 6D 7H 3H 4H 5H 6H 7H", False)]       

cards = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']
suits = ['C', 'H', 'S', 'D']

readCard :: String -> (Int,Int)
readCard (c:s:[]) = ((fromJust $ elemIndex c cards) + 2, fromJust $ elemIndex s suits)

readHands line = let cards = map readCard $ words line 
                 in (take 5 cards, drop 5 cards)

isFlush :: [(Int,Int)] -> Bool
isFlush hand = let values = map snd hand
               in and $ map (==(head values)) $ tail values

isStraight hand = let sorted = map fst $ sortCards hand
                  in (((sorted !! 0) - (sorted !! 4) == 4) && ((sorted !! 0) - (sorted !! 3) == 3) &&
				      ((sorted !! 0) - (sorted !! 2) == 2) && ((sorted !! 0) - (sorted !! 1) == 1))

sortCards hand = reverse $ sortBy (compare `on` fst) hand

groupCards hand = reverse $ sortBy (compare `on` length) $ groupBy ((==) `on` fst) $ sortCards hand

combinations hand = 
        let sortedHand = sortCards hand
            flush = isFlush hand
            straight = isStraight hand
            groups = groupCards hand
            numGroups = length groups
            fstGroup = head groups
            fstLenght = length fstGroup
            sndGroup = groups !! 1
            sndLenght = length sndGroup
            restGroups = tail groups
            rest = sortCards $ concat restGroups
        in 
           [(flush && straight, sortedHand)
           ,(fstLenght == 4, fstGroup ++ rest)
           ,(numGroups >= 2 && fstLenght == 3 && sndLenght == 2, fstGroup ++ sndGroup)
           ,(flush, sortedHand)
           ,(straight, sortedHand)
           ,(fstLenght == 3, fstGroup ++ rest)
           ,(numGroups >= 2 && fstLenght == 2 && sndLenght == 2, (sortCards $ fstGroup ++ sndGroup) ++ (groups !! 2))
           ,(fstLenght == 2, fstGroup ++ rest)
           ,(True, sortedHand)
           ]

bestCombination hand = let allCombinations = combinations hand
                           matched = filter (\((passed, _), n) -> passed) $ zip allCombinations [9,8..1]
                           ((_, groups), score) = head matched                           
                       in (score, map fst groups)
                       
compareHands (hand1,hand2) = 
        let (score1, cards1) = bestCombination hand1
            (score2, cards2) = bestCombination hand2
        in if score1 == score2 then cards1 > cards2
                               else score1 > score2
                               
check = map (\(hands, result) -> compareHands hands == result) testSet

main = do raw <- readFile "54.txt"
          let hands = map readHands $ lines raw
          let result = length $ filter id $ map compareHands hands
          putStrLn $ show result