{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Nat.Size (
    termSizeInput
  ) where

import Control.Lens (preview)

import Component.Term.Size (TermSizeInput(..), TermSizeRule(..))

import Component.Term.Nat (AsNatTerm(..), WithNatTerm)

termSizeTmZero :: WithNatTerm tm n a
               => tm n a
               -> Maybe Int
termSizeTmZero =
  fmap (const 1) .
  preview _TmZero

termSizeTmSucc :: WithNatTerm tm n a
               => (tm n a -> Int)
               -> tm n a
               -> Maybe Int
termSizeTmSucc size =
  fmap ((+ 1) . size) .
  preview _TmSucc

termSizeTmPred :: WithNatTerm tm n a
               => (tm n a -> Int)
               -> tm n a
               -> Maybe Int
termSizeTmPred size =
  fmap ((+ 1) . size) .
  preview _TmPred

termSizeInput :: WithNatTerm tm n a
              => TermSizeInput tm n a
termSizeInput =
  TermSizeInput
    [ TermSizeBase termSizeTmZero
    , TermSizeRecurse termSizeTmSucc
    , TermSizeRecurse termSizeTmPred
    ]
