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
module Component.Term.Size (
    TermSizeRule(..)
  , TermSizeInput(..)
  , TermSizeOutput
  , HasTermSizeOutput(..)
  , mkTermSize
  ) where

import           Control.Lens.TH (makeClassy)
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe)

-- |
data TermSizeRule tm =
    TermSizeBase (tm -> Maybe Int)                   -- ^
  | TermSizeRecurse ((tm -> Int) -> tm -> Maybe Int) -- ^

-- |
fixTermSizeRule :: (tm -> Int)
                  -> TermSizeRule tm
                  -> tm
                  -> Maybe Int
fixTermSizeRule _ (TermSizeBase f) x =
  f x
fixTermSizeRule step (TermSizeRecurse f) x =
  f step x

-- |
data TermSizeInput tm =
  TermSizeInput [TermSizeRule tm] -- ^

instance Monoid (TermSizeInput tm) where
  mempty =
    TermSizeInput mempty
  mappend (TermSizeInput v1) (TermSizeInput v2) =
    TermSizeInput (mappend v1 v2)

-- |
data TermSizeOutput tm =
  TermSizeOutput {
    _termSize :: tm -> Int -- ^
  }

makeClassy ''TermSizeOutput

-- |
mkTermSize :: TermSizeInput tm  -- ^
            -> TermSizeOutput tm -- ^
mkTermSize (TermSizeInput i) =
  let
    termSizeRules' =
      fmap (fixTermSizeRule termSize') i
    termSize' tm =
      fromMaybe 0 .
      asum .
      fmap ($ tm) $
      termSizeRules'
  in
    TermSizeOutput
      termSize'
