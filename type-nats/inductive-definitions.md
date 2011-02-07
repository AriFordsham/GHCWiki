```wiki
{-# LANGUAGE TypeNaturals, GADTs #-}

import GHC.TypeNats
import Unsafe.Coerce

--------------------------------------------------------------------------------
-- Extending GHC.TypeNats with these two declarations allows us to
-- write inductive definitions.

data UNat :: Nat -> * where
  Zero :: UNat 0
  Succ :: UNat n -> UNat (n + 1)

toUNat :: Nat n -> UNat n
toUNat n = unsafe (natToInteger n)
  where unsafe :: Integer -> UNat n
        unsafe 0 = unsafeCoerce Zero
        unsafe n = unsafeCoerce (Succ (unsafe $! (n-1)))

--------------------------------------------------------------------------------

data Vec :: Nat -> * -> * where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a

instance Show a => Show (Vec n a) where
  show Nil = "[]"
  show (Cons x xs) = show x ++ " : " ++ show xs

instance Functor (Vec n) where
  fmap f Nil          = Nil
  fmap f (Cons x xs)  = Cons (f x) (fmap f xs)

cat :: Vec m a -> Vec n a -> Vec (m + n) a
cat Nil ys          = ys
cat (Cons x xs) ys  = Cons x (cat xs ys)

vecLen :: NatI n => Vec n a -> Nat n
vecLen _ = nat


splitU :: UNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
splitU Zero xs               = (Nil, xs)
splitU (Succ n) (Cons x xs)  = let (as,bs) = splitU n xs
                               in (Cons x as, bs)

vecSplitAt :: Nat m -> Vec (m + n) a -> (Vec m a, Vec n a)
vecSplitAt n = splitU (toUNat n)

vecSplit :: NatI m => Vec (m + n) a -> (Vec m a, Vec n a)
vecSplit = vecSplitAt nat
```