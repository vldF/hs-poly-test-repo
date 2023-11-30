module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin = notImplementedYet

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos = notImplementedYet

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD = notImplementedYet

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?

isYearLeap y
            | y `mod` 400 == 0 = True
            | y `mod` 100 == 0 = False
            | y `mod` 4 == 0 = True
            | otherwise = False

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y = case m of
    1 -> d <= 31
    2 -> (if isYearLeap y then d <= 29 else d <= 28)
    3 -> d <= 31
    4 -> d <= 30
    5 -> d <= 31
    6 -> d <= 30
    7 -> d <= 31
    8 -> d <= 31
    9 -> d <= 30
    10 -> d <= 31
    11 -> d <= 30
    12 -> d <= 31
    otherwise -> False


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow num 0 = 1
myPow num pow = num * myPow num (pow - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n = (length [x | x <- [1..n], mod n x == 0]) == 2

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = abs((shapeAreaInternal (points ++ take 1 points) - shapeAreaInternal (invert (points ++ take 1 points))) / 2.0)
-- shapeArea = notImplementedYet

invert ((a,b):tail) = (b,a) : invert tail
invert [] = []

shapeAreaInternal :: [Point2D] -> Double
shapeAreaInternal [] = 0.0
shapeAreaInternal [point] = 0.0
shapeAreaInternal points = fst (head points) * snd (points!!1) + shapeAreaInternal (drop 1 points)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = let greatest = max a (max b c)
                         lowest = min a (min b c)
                         middlest = a + b + c - greatest - lowest
                         isImpossible = greatest > lowest + middlest
                         isRight = greatest ** 2.0 == lowest ** 2.0 + middlest ** 2
                         isObtuse = greatest ** 2.0 < lowest ** 2.0 + middlest ** 2
                     in case () of 
                       _ | isImpossible -> -1
                         | isRight -> 2
                         | isObtuse -> 1
                         | otherwise -> 0


    
