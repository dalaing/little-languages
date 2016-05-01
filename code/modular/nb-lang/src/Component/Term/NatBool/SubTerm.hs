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
                => (tm n a -> [tm n a])
                -> tm n a
                -> Maybe [tm n a]
subTermTmIsZero subTerms tm =
  fmap ((tm :) . subTerms) .
  preview _TmIsZero $
  tm

subTermInput :: WithNatBoolTerm tm
             => SubTermInput tm n a
subTermInput =
  SubTermInput
    [SubTermRecurse subTermTmIsZero]
