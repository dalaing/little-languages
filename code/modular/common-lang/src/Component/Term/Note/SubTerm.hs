{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables             #-}
module Component.Term.Note.SubTerm (
    subTermInput
  ) where

import Control.Lens (preview)

import Component.Term.SubTerm (SubTermInput(..), SubTermRule(..))

import Component.Term.Note (AsNoteTerm(..), WithNoteTerm)

subTermTmNote :: WithNoteTerm tm
              => (tm nTy nTm a -> [tm nTy nTm a])
              -> tm nTy nTm a
              -> Maybe [tm nTy nTm a]
subTermTmNote subTerms tm =
  fmap ((tm :) . subTerms . snd) .
  preview _TmNote $
  tm

subTermInput :: WithNoteTerm tm
              => SubTermInput tm nTy nTm a
subTermInput =
  SubTermInput
    [SubTermRecurse subTermTmNote]
