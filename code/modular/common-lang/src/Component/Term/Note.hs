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
module Component.Term.Note (
    NoteTerm(..)
  , AsNoteTerm(..)
  , WithNoteTerm
  ) where

import           Control.Lens.TH (makeClassyPrisms)

import           Bound           (Bound (..))

data NoteTerm n tm a =
  TmNote n (tm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NoteTerm

instance Bound (NoteTerm n) where
  TmNote n tm >>>= f = TmNote n (tm >>= f)

type WithNoteTerm tm n a = AsNoteTerm (tm a) n tm a
