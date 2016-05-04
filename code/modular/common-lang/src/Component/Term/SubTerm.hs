{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Component.Term.SubTerm (
    SubTermRule(..)
  , SubTermInput(..)
  , SubTermOutput
  , HasSubTermOutput(..)
  , mkSubTerm
  ) where

import           Control.Lens.TH (makeClassy)
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe)

-- |
data SubTermRule tm nTy nTm a =
    SubTermBase (tm nTy nTm a -> Maybe [tm nTy nTm a])                   -- ^
  | SubTermRecurse ((tm nTy nTm a -> [tm nTy nTm a]) -> tm nTy nTm a -> Maybe [tm nTy nTm a]) -- ^

-- |
fixSubTermRule :: (tm nTy nTm a -> [tm nTy nTm a])
                  -> SubTermRule tm nTy nTm a
                  -> tm nTy nTm a
                  -> Maybe [tm nTy nTm a]
fixSubTermRule _ (SubTermBase f) x =
  f x
fixSubTermRule step (SubTermRecurse f) x =
  f step x

-- |
data SubTermInput tm nTy nTm a =
  SubTermInput [SubTermRule tm nTy nTm a] -- ^

instance Monoid (SubTermInput tm nTy nTm a) where
  mempty =
    SubTermInput mempty
  mappend (SubTermInput v1) (SubTermInput v2) =
    SubTermInput (mappend v1 v2)

-- |
data SubTermOutput tm nTy nTm a =
  SubTermOutput {
    _subTerms :: tm nTy nTm a -> [tm nTy nTm a]
  , _containsTerm :: tm nTy nTm a -> tm nTy nTm a -> Bool
  , _termSize :: tm nTy nTm a -> Int -- ^
  }

makeClassy ''SubTermOutput

-- |
mkSubTerm :: Eq (tm nTy nTm a)
          => SubTermInput tm nTy nTm a  -- ^
          -> SubTermOutput tm nTy nTm a -- ^
mkSubTerm (SubTermInput i) =
  let
    subTermRules' =
      fmap (fixSubTermRule subTerms') i
    subTerms' tm =
      fromMaybe [] .
      asum .
      fmap ($ tm) $
      subTermRules'
    containsTerm' x y = elem y . subTerms' $ x
    termSize' = length . subTerms'
  in
    SubTermOutput
      subTerms'
      containsTerm'
      termSize'
