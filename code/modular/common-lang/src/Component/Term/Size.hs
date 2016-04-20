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
data TermSizeRule tm n a =
    TermSizeBase (tm n a -> Maybe Int)                   -- ^
  | TermSizeRecurse ((tm n a -> Int) -> tm n a -> Maybe Int) -- ^

-- |
fixTermSizeRule :: (tm n a -> Int)
                  -> TermSizeRule tm n a
                  -> tm n a
                  -> Maybe Int
fixTermSizeRule _ (TermSizeBase f) x =
  f x
fixTermSizeRule step (TermSizeRecurse f) x =
  f step x

-- |
data TermSizeInput tm n a =
  TermSizeInput [TermSizeRule tm n a] -- ^

instance Monoid (TermSizeInput tm n a) where
  mempty =
    TermSizeInput mempty
  mappend (TermSizeInput v1) (TermSizeInput v2) =
    TermSizeInput (mappend v1 v2)

-- |
data TermSizeOutput tm n a =
  TermSizeOutput {
    _termSize :: tm n a -> Int -- ^
  }

makeClassy ''TermSizeOutput

-- |
mkTermSize :: TermSizeInput tm n a  -- ^
            -> TermSizeOutput tm n a -- ^
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
