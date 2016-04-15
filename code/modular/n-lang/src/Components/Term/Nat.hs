{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Components.Term.Nat (
    NatTerm(..)
  , AsNatTerm(..)
  , WithNatTerm
  ) where

import           Control.Lens.TH      (makeClassyPrisms)
import Bound (Bound(..))

-- |
data NatTerm tm a =
    TmZero         -- ^
  | TmSucc (tm a)  -- ^
  | TmPred (tm a)  -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NatTerm

instance Bound NatTerm where
  TmZero    >>>= _ = TmZero
  TmSucc tm >>>= f = TmSucc (tm >>= f)
  TmPred tm >>>= f = TmPred (tm >>= f)

type WithNatTerm tm a = AsNatTerm (tm a) tm a
