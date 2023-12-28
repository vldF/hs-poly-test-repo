{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map
import qualified Data.Tree as Prelude
import qualified Data.Aeson.KeyMap as Map
import Debug.Trace (trace)


-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       -- Реализуйте следующие функции
       -- Единичная матрица
       eye :: Int -> mx

       -- Матрица, заполненная нулями
       zero :: Int -> Int -> mx

       -- Перемножение матриц
       multiplyMatrix :: mx -> mx -> mx

       -- Определитель матрицы
       determinant ::  mx -> Int

       element :: mx -> Int -> Int -> Int
       column :: mx -> Int -> [Int]
       row :: mx -> Int -> [Int]

       columns :: mx -> Int
       rows :: mx -> Int


-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       eye _ = 1
       zero _ _ = 0

       multiplyMatrix m1 m2 = m1 * m2
       determinant m = m

       element mx _ _ = mx
       column mx _ = [mx]
       row mx _ = [mx]

       columns _ = 1
       rows _ = 1

instance Matrix [[Int]] where
       eye i = Prelude.map (\x -> (replicate x 0 ++ [1]) ++ replicate (i - x - 1) 0) [0..(i-1)]
       zero h w = replicate w (replicate h 0)

       element mx y x = mx !! x !! y
       column mx i = Prelude.map (!! i) mx
       row mx i = mx !! i

       multiplyMatrix m1 m2 = Prelude.map (\m1row -> Prelude.map (\m2col -> dot (row m1 m1row) (column m2 m2col)) [0..(columns m2 - 1)]) [0..(rows m1 - 1)]
       determinant m = notImplementedYet

       columns mx = length (head mx)
       rows = length

instance Matrix (SparseMatrix Int) where
       eye i = SparseMatrix w h (Data.Map.fromList d) where
              w = i
              h = i
              d = Prelude.map (\x -> ((x, x), 1)) [0..(i-1)]

       zero h w = SparseMatrix w h Data.Map.empty

       element mx x y = if member (x, y) (sparseMatrixElements mx) then sparseMatrixElements mx ! (x, y) else 0
       column mx i = Prelude.map (element mx i) [0..(sparseMatrixHeight mx)]
       row mx i = Prelude.map (\x -> element mx x i) [0..(sparseMatrixWidth mx)]

       multiplyMatrix m1 m2 = SparseMatrix w h (Data.Map.fromList (Prelude.concat d)) where
              w = sparseMatrixWidth m2
              h = sparseMatrixHeight m1
              d = Prelude.map (\m1row -> Prelude.map (\m2col -> ((m2col, m1row), dot (row m1 m1row) (column m2 m2col))) [0..(columns m2 - 1)]) [0..(rows m1 - 1)]
       determinant m = notImplementedYet
       columns = sparseMatrixWidth
       rows = sparseMatrixHeight

dot :: [Int] -> [Int] -> Int
dot v1 v2 = sum (zipWith (*) v1 v2)

sMatrixToMatrix mx = Prelude.map (\column -> Prelude.map (element mx column) [0..(rows mx - 1)]) [0..(columns mx - 1)]
