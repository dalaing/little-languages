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

import Component.Term.SubTerm (SubTermInput(..), SubTermRule(..))

import Component.Term.Note (AsNoteTerm(..), WithNoteTerm)

subTermTmNote :: WithNoteTerm tm
              => (tm n a -> [tm n a])
              -> tm n a
              -> Maybe [tm n a]
subTermTmNote subTerms tm =
  fmap ((tm :) . subTerms . snd) .
  preview _TmNote $
  tm

termSizeInput :: WithNoteTerm tm
              => SubTermInput tm n a
termSizeInput =
  SubTermInput
    [SubTermRecurse subTermTmNote]
