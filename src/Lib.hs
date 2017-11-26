{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Data.Generics.Product
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

-- GRowToList from kcsongor
type family GRowToList (r :: * -> *) :: [(Symbol, *)] where
  GRowToList (l :*: r)
    = GRowToList l ++ GRowToList r
  GRowToList (S1 ('MetaSel ('Just name) _ _ _) (Rec0 a))
    = '[ '(name, a) ]
  GRowToList (M1 _ m a)
    = GRowToList a
  GRowToList U1 = '[]

type RowToList t = Sort (GRowToList (Rep t))

-- Ord class for kinds
class Ord' k where
  type Cmp (a :: k) (b :: k) :: Ordering

instance Ord' Symbol where
  type Cmp a b = CmpSymbol a b

instance Ord' Nat where
  type Cmp a b = CmpNat a b

instance Ord' k => Ord' (k, j) where
  type Cmp '(a, x) '(b, y) = Cmp a b

-- Sorting lists
type family Sort (xs :: [k]) where
  Sort (x ': xs) = Insert x (Sort xs)
  Sort '[] = '[]

type family Insert (x :: k) (xs :: [k]) where
  Insert x (y ': ys) = Insert' (Cmp x y) x y ys
  Insert x '[] = '[x]

type family Insert' (ord :: Ordering) (x :: k) (y :: k) (ys :: [k]) where
  Insert' 'LT x y ys = x ': y ': ys
  Insert' _   x y ys = y ': Insert x ys

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

class PairwiseApply
    functions (functionsList :: [(Symbol, *)])
    values (valuesList :: [(Symbol, *)])
  where
    pairwiseApplyImpl
      :: Proxy functionsList
      -> Proxy valuesList
      -> functions
      -> values
      -> values

instance PairwiseApply functions '[] values '[] where
  pairwiseApplyImpl _ _ _ x = x

instance
  ( HasField name (val -> val) functions
  , HasField name val values
  , PairwiseApply functions fnTail values valTail
  ) => PairwiseApply
         functions (('(name, (val -> val))) ': fnTail)
         values ('(name, val) ': valTail) where
  pairwiseApplyImpl _ _ fns vals = do
      setField @name (fn val) vals'
    where
      fn = getField @name fns
      val = getField @name vals
      vals' = pairwiseApplyImpl (Proxy @fnTail) (Proxy @valTail) fns vals

pairwiseApply :: forall vals fns valsL fnsL
   . Generic fns
  => Generic vals
  => fnsL ~ RowToList fns
  => valsL ~ RowToList vals
  => PairwiseApply
       fns fnsL
       vals valsL
  => fns
  -> vals
  -> vals
pairwiseApply =
  pairwiseApplyImpl
    (Proxy @fnsL)
    (Proxy @valsL)
