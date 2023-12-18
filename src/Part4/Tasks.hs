module Part4.Tasks where

import Util(notImplementedYet)
import Control.Applicative (liftA2)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldr (\ head -> (<>) (REmpty :< head)) REmpty

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show lst = "[" <> show_ lst <> "]" where
        show_ REmpty = ""
        show_ (REmpty :< head) = show head
        show_ (tail :< head) = show_ tail <> "," <> show head

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) REmpty _ = False
    (==) _ REmpty = False
    (==) (tail1 :< head1) (tail2 :< head2) = head1 == head2 && tail1 == tail2

    (/=) l1 l2 = not (l1 == l2)

instance Semigroup (ReverseList a) where
    (<>) fst REmpty = fst
    (<>) fst (h :< t) = (fst <> h) :< t

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap f REmpty = REmpty
    fmap f (h :< t) = fmap f h :< f t

instance Applicative ReverseList where
    pure a = REmpty :< a
    liftA2 _ REmpty _ = REmpty
    liftA2 _ _ REmpty = REmpty
    liftA2 f (h :< t) x  = liftA2 f h x <> fmap (f t) x

instance Monad ReverseList where
  (>>=) REmpty f = REmpty
  (>>=) (h :< t) f = (h >>= f) <> f t
