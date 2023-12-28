module Part6.Tests where

import qualified Data.Map

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Part6.Tasks

unit_eye = do
    eye 1 @?= one
    eye 1 @?= [[one]]
    eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
    eye 2 @?= [[one, 0], [0, one]]
    eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])

    where one :: Int; one = 1

unit_zero = do
    zero 1 1 @?= zz
    zero 2 1 @?= [[zz, zz]]
    zero 2 2 @?= [[zz, zz], [zz, zz]]
    zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([]::[((Int, Int), Int)]))
    where zz :: Int; zz = 0

unit_mult_scalars = do
    multiplyMatrix m1 m2 @?= 1 * 2
    multiplyMatrix m2 m3 @?= 2 * 10
    where
        m1 :: Int;
        m2 :: Int;
        m3 :: Int
        m1 = 1;
        m2 = 2;
        m3 = 10;

unit_mult_matrix = do
    multiplyMatrix m1 m2 @?= [[1 * 1 + 2 * 2 + 3 * 3]]
    multiplyMatrix m1 m3 @?= [[1 * 1 + 2 * 4 + 3 * 7, 1 * 2 + 2 * 5 + 3 * 8, 1 * 3 + 2 * 6 + 3 * 9]]
    multiplyMatrix m3 m2 @?= [[1 * 1 + 2 * 2 + 3 * 3], [4 * 1 + 5 * 2 + 6 * 3], [7 * 1 + 8 * 2 + 9 * 3]]
    where
        m1 :: [[Int]];
        m2 :: [[Int]];
        m3 :: [[Int]]
        m1 = [[1, 2, 3]];
        m2 = [[1], [2], [3]];
        m3 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

unit_sMatrixToMatrix = do
    sMatrixToMatrix m1 @?= l1
    sMatrixToMatrix m2 @?= l2
    sMatrixToMatrix m3 @?= l3

    where
        m1 :: SparseMatrix Int;
        m2 :: SparseMatrix Int;
        m3 :: SparseMatrix Int;

        m1 = SparseMatrix 2 2 (Data.Map.fromList [((0, 0), 1), ((1, 1), 10)])
        m2 = SparseMatrix 3 3 (Data.Map.fromList [((0, 0), 1), ((0, 1), 2), ((0, 2), 3), ((1, 0), 4), ((1, 1), 5), ((1, 2), 6), ((2, 0), 7), ((2, 1), 8), ((2, 2), 9)])
        m3 = SparseMatrix 1 3 (Data.Map.fromList [((0, 0), 1), ((0, 1), 2), ((0, 2), 3)])
        l1 = [[1, 0], [0, 10]]
        l2 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        l3 = [[1, 2, 3]]

unit_mult_s_matrix = do
    sMatrixToMatrix (multiplyMatrix m1 m2) @?= [[3], [3], [0]]
    sMatrixToMatrix (multiplyMatrix m1 m2) @?= [[3], [3], [0]]
    sMatrixToMatrix (multiplyMatrix m3 m1) @?= [[1,0,1],[0,0,0],[1,0,1]]

    where
        m1 :: SparseMatrix Int;
        m2 :: SparseMatrix Int;
        -- m3 :: SparseMatrix Int
        m1 = SparseMatrix 3 1 (Data.Map.fromList [((0, 0), 1), ((2, 0), 1)]);
        m2 = SparseMatrix 3 3 (Data.Map.fromList [((0, 0), 1), ((1, 0), 2), ((1, 1), 3), ((2, 1), 1), ((0, 2), 2), ((1, 2), 1)]);
        m3 = SparseMatrix 1 3 (Data.Map.fromList [((0, 0), 1), ((0, 2), 1)]);
