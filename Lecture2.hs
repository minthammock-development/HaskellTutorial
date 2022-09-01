module Lecture2 where
import GHC.Base ()

eval :: Char -> Int -> Int -> Int
eval op x y = case op of
    '+' -> x + y
    '-' -> x - y 
    '*' -> x * y
    '/' -> div x y 
    _ -> 0

sumOfTwoInThree :: [Int] -> Int
sumOfTwoInThree [x, _, y] = x + y
sumOfTwoInThree _ = 0

oneOrTwoZeroes :: [Int] -> Bool
oneOrTwoZeroes list = case list of
    [0] -> True
    [0,0] -> True
    _ -> False

headOrDef :: Int -> [Int] -> Int
headOrDef def [] = def
headOrDef _ (x : _) = x

dropHead :: [Int] -> [Int]
dropHead [] = []
dropHead (_ : xs) = xs

secondIsZero :: [Int] -> Bool
secondIsZero list =  case list of 
    (_ : 0 : _) -> True
    _ ->  False