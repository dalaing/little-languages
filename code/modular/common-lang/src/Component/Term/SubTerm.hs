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
data SubTermRule tm n a =
    SubTermBase (tm n a -> Maybe [tm n a])                   -- ^
  | SubTermRecurse ((tm n a -> [tm n a]) -> tm n a -> Maybe [tm n a]) -- ^

-- |
fixSubTermRule :: (tm n a -> [tm n a])
                  -> SubTermRule tm n a
                  -> tm n a
                  -> Maybe [tm n a]
fixSubTermRule _ (SubTermBase f) x =
  f x
fixSubTermRule step (SubTermRecurse f) x =
  f step x

-- |
data SubTermInput tm n a =
  SubTermInput [SubTermRule tm n a] -- ^

instance Monoid (SubTermInput tm n a) where
  mempty =
    SubTermInput mempty
  mappend (SubTermInput v1) (SubTermInput v2) =
    SubTermInput (mappend v1 v2)

-- |
data SubTermOutput tm n a =
  SubTermOutput {
    _subTerms :: tm n a -> [tm n a]
  , _containsTerm :: tm n a -> tm n a -> Bool
  , _termSize :: tm n a -> Int -- ^
  }

makeClassy ''SubTermOutput

-- |
mkSubTerm :: Eq (tm n a)
          => SubTermInput tm n a  -- ^
          -> SubTermOutput tm n a -- ^
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
