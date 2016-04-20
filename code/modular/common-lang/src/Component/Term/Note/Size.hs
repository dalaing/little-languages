{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables             #-}
module Component.Term.Note.Size (
    termSizeInput
  ) where

import Control.Lens (preview)

import Component.Term.Size (TermSizeInput(..), TermSizeRule(..))

import Component.Term.Note (AsNoteTerm(..), WithNoteTerm)

termSizeTmNote :: WithNoteTerm tm n a
               => (tm n a -> Int)
               -> tm n a
               -> Maybe Int
termSizeTmNote size =
  fmap ((+ 1) . size . snd) .
  preview _TmNote

termSizeInput :: WithNoteTerm tm n a
              => TermSizeInput tm n a
termSizeInput =
  TermSizeInput
    [TermSizeRecurse termSizeTmNote]
