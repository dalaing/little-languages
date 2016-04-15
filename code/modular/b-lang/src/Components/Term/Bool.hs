{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Components.Term.Bool (
    BoolTerm(..)
  , AsBoolTerm(..)
  , WithBoolTerm
  ) where

import           Control.Lens.TH (makeClassyPrisms)

import Bound (Bound(..))

-- |
data BoolTerm tm a =
    TmFalse       -- ^
  | TmTrue        -- ^
  | TmIf (tm a) (tm a) (tm a) -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''BoolTerm

instance Bound BoolTerm where
  TmFalse          >>>= _ = TmFalse
  TmTrue           >>>= _ = TmTrue
  TmIf tm1 tm2 tm3 >>>= f = TmIf (tm1 >>= f) (tm2 >>= f) (tm3 >>= f)

type WithBoolTerm tm a = AsBoolTerm (tm a) tm a
