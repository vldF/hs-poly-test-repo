module Part3.Tasks where

import Util (notImplementedYet)
import Data.List (sort, group, sortBy, groupBy, nub)
import Data.Function (on)
import Control.Arrow (Arrow(second))

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = [f x | x <- [n..]]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ffApply :: (a -> a) -> a -> Integer -> a
ffApply f a 0 = a
ffApply f a i = f (ffApply f a (i - 1))

ff :: (a -> a) -> a -> [a]
ff f x = [ffApply f x n  | n <- [0..]]

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq list = let allDidits = group (concatMap digits list)
                    tmp = sortBy (compare `on` length) allDidits
                    result = head (last tmp)
                in result

digits :: Int -> [Int]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)

sumStats :: [Int] -> [Int] -> [Int]
sumStats (h1:t1) (h2:t2) = h1 + h2 : sumStats t1 t2
sumStats [] [] = []

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = nub

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = map (\lst -> (fst (head lst), map snd lst)) (groupBy (\e1 e2 -> fst e1 == fst e2) (map (\e -> (f e, e)) l))