{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.NatBool.Size (
    termSizeInput
  ) where

import Control.Lens (preview)

import Component.Term.Size (TermSizeInput(..), TermSizeRule(..))

import Component.Term.NatBool (AsNatBoolTerm(..), WithNatBoolTerm)

termSizeTmIsZero :: WithNatBoolTerm tm
                 => (tm n a -> Int)
                 -> tm n a
                 -> Maybe Int
termSizeTmIsZero size =
  fmap ((+ 1) . size) .
  preview _TmIsZero

termSizeInput :: WithNatBoolTerm tm
              => TermSizeInput tm n a
termSizeInput =
  TermSizeInput
    [TermSizeRecurse termSizeTmIsZero]
