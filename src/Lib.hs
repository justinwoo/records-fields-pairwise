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
  => fnsL ~ GRowToList (Rep fns)
  => valsL ~ GRowToList (Rep vals)
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
