module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 7 |+|
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 7 |-|
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 8 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
                                                IntConstant _ -> expression
                                                BinaryTerm op lhv rhv ->
                                                   BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)
                                                Variable name -> if name == varName then replacement else expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
   IntConstant _ -> expression
   Variable _ -> expression
   BinaryTerm op lhv rhv -> case (evaluate lhv, evaluate rhv) of
      (IntConstant i1, IntConstant i2) -> case op of 
         Plus -> IntConstant (i1 + i2)
         Minus -> IntConstant (i1 - i2)
         Times -> IntConstant (i1 * i2)
      (t1, t2) -> BinaryTerm op t1 t2 
