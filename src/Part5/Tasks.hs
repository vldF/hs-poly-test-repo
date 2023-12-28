module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f arg (head:tail) = myFoldl f t tail where
    t = f arg head

myFoldl _ arg [] = arg

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f arg (head:tail) = f head (myFoldr f arg tail)
myFoldr _ arg [] = arg

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr mapper [] where
    mapper new acc = f new : acc

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr mapper [] where
    mapper new acc = f new ++ acc

myConcat :: [[a]] -> [a]
myConcat = myConcatMap id

myReverse :: [a] -> [a]
myReverse = myFoldl concat [] where
    concat :: [a] -> a -> [a]
    concat acc new = new : acc

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr filter_ [] where
    filter_ new acc = if p new
        then new : acc
        else acc

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p lst = (myFilter p lst, myFilter (not . p) lst)

