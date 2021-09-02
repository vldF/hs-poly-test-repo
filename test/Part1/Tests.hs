module Part1.Tests where

import Test.Tasty.HUnit

import Part1.Tasks
import Util

unit_myFoldl = do
    myFoldl (+) 0 [1, 2, 3] @?= 6

prop_myFoldl :: Int -> [Int] -> Bool
prop_myFoldl a as = myFoldl (+) a as == foldl (+) a as

unit_myFoldr = do
    myFoldr (+) 0 [1, 2, 3] @?= 6

prop_myFoldr :: String -> [String] -> Bool
prop_myFoldr a as = myFoldr (++) a as == foldr (++) a as

unit_myMap = do
    assertEqual "" [] (myMap (+45) [])
    assertEqual "" [2,3,4] (myMap (+1) [1,2,3])

prop_myMap :: String -> [String] -> Bool
prop_myMap suffix lst = myMap (++suffix) lst == map (++suffix) lst

unit_myReverse = do
    assertEqual "" empty (myReverse empty)
    assertEqual "" [1] (myReverse [1])
    assertEqual "" [1,2,3] (myReverse [3, 2, 1])
        where empty = [] :: [Int]

prop_myReverse lst = myReverse (myReverse lst) == lst
    where types = lst :: [String]

unit_myConcat = do
    assertEqual "" "abc" (myConcat ["a", "b", "c"])

unit_myFilter = do
    myFilter (`elem` "123456789") "a1v234asas5" @?= "12345"

prop_myFilter :: [Int] -> Bool
prop_myFilter lst = myFilter (\x -> x `mod` 2 == 1) lst == filter (\x -> x `mod` 2 == 1) lst
