{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.NatBool.SubTerm (
    subTermInput
  ) where

import Control.Lens (preview)

import Component.Term.SubTerm (SubTermInput(..), SubTermRule(..))

import Component.Term.NatBool (AsNatBoolTerm(..), WithNatBoolTerm)

subTermTmIsZero :: WithNatBoolTerm tm
                => (tm nTy nTm a -> [tm nTy nTm a])
                -> tm nTy nTm a
                -> Maybe [tm nTy nTm a]
subTermTmIsZero subTerms tm =
  fmap ((tm :) . subTerms) .
  preview _TmIsZero $
  tm

subTermInput :: WithNatBoolTerm tm
             => SubTermInput tm nTy nTm a
subTermInput =
  SubTermInput
    [SubTermRecurse subTermTmIsZero]
