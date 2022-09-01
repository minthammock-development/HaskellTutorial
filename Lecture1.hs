module Lecture1 where
import GHC.Base (VecElem(Int16ElemRep))
    
increase :: Integer -> Integer
increase x = x + 1

headOrDefault :: Int -> [Int] -> Int
headOrDefault def list = if null list then def else head list

sign :: Int -> String -- this is an example of a guard
sign n
    | n == 0 = "Zero"
    | n < 0 = "negative"
    | otherwise = "positive"

sameThreeAround :: [Int] -> Bool -- let-in example
sameThreeAround list = 
    let firstThree = take 3 list
        lastThree = reverse (take 3 (reverse list))
    in firstThree == lastThree

appendLastTwos :: [Int] -> [Int] -> [Int]
appendLastTwos list1 list2 = lastTwo list1 ++ lastTwo list2
    where
        lastTwo :: [Int] -> [Int]
        lastTwo list = reverse (take 2 (reverse list))

count :: Int -> [Int] -> Int
count n list = go 0 list
    where
        go :: Int -> [Int] -> Int
        go result list =
            if null list
            then result
            else if head list == n
                then go (result +1) (tail list)
                else go result (tail list)

applyToSame :: (Int -> Int -> Int) -> Int -> Int
applyToSame f x = f x x

satisfies :: (Int -> Bool) -> Int -> String
satisfies check n
    | check n = "The number " ++ show n ++ " passes check"
    | otherwise = "The number " ++ show n ++ " doesn't pass"

applyTwice :: (Int -> Int) -> Int -> Int
applyTwice f x = f (f x)

