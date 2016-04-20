{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Bool.Size (
    termSizeInput
  ) where

import Control.Lens (preview)

import Component.Term.Size (TermSizeInput(..), TermSizeRule(..))

import Component.Term.Bool (AsBoolTerm(..), WithBoolTerm)

termSizeTmFalse :: WithBoolTerm tm n a
                => tm n a
                -> Maybe Int
termSizeTmFalse =
  fmap (const 1) .
  preview _TmFalse

termSizeTmTrue :: WithBoolTerm tm n a
               => tm n a
               -> Maybe Int
termSizeTmTrue =
  fmap (const 1) .
  preview _TmTrue

termSizeTmIf :: WithBoolTerm tm n a
              => (tm n a -> Int)
              -> tm n a
              -> Maybe Int
termSizeTmIf size =
  fmap (\(tm1, tm2, tm3) -> 1 + size tm1 + size tm2 + size tm3) .
  preview _TmIf

termSizeInput :: WithBoolTerm tm n a
              => TermSizeInput tm n a
termSizeInput =
  TermSizeInput
    [ TermSizeBase termSizeTmFalse
    , TermSizeBase termSizeTmTrue
    , TermSizeRecurse termSizeTmIf
    ]
