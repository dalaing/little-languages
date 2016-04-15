{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Nat.Size (
    termSizeInput
  ) where

import Control.Lens (preview)

import Component.Term.Size (TermSizeInput(..), TermSizeRule(..))

import Components.Term.Nat (AsNatTerm(..), WithNatTerm)

termSizeTmZero :: WithNatTerm tm a
               => tm a
               -> Maybe Int
termSizeTmZero =
  fmap (const 1) .
  preview _TmZero

termSizeTmSucc :: WithNatTerm tm a
               => (tm a -> Int)
               -> tm a
               -> Maybe Int
termSizeTmSucc size =
  fmap ((+ 1) . size) .
  preview _TmSucc

termSizeTmPred :: WithNatTerm tm a
               => (tm a -> Int)
               -> tm a
               -> Maybe Int
termSizeTmPred size =
  fmap ((+ 1) . size) .
  preview _TmPred

termSizeInput :: WithNatTerm tm a
              => TermSizeInput (tm a)
termSizeInput =
  TermSizeInput
    [ TermSizeBase termSizeTmZero
    , TermSizeRecurse termSizeTmSucc
    , TermSizeRecurse termSizeTmPred
    ]
