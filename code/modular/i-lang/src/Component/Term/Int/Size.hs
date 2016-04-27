{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Int.Size (
    termSizeInput
  ) where

import Control.Lens (preview)

import Component.Term.Size (TermSizeInput(..), TermSizeRule(..))

import Component.Term.Int (AsIntTerm(..), WithIntTerm)

termSizeTmInt :: WithIntTerm tm
               => tm n a
               -> Maybe Int
termSizeTmInt =
  fmap (const 1) .
  preview _TmIntLit

termSizeTmAdd :: WithIntTerm tm
              => (tm n a -> Int)
              -> tm n a
              -> Maybe Int
termSizeTmAdd size =
  fmap termSizeTmAdd' .
  preview _TmAdd
  where
    termSizeTmAdd' (x, y) =
      1 + size x + size y

termSizeTmSub :: WithIntTerm tm
              => (tm n a -> Int)
              -> tm n a
              -> Maybe Int
termSizeTmSub size =
  fmap termSizeTmSub' .
  preview _TmSub
  where
    termSizeTmSub' (x, y) =
      1 + size x + size y

termSizeTmMul :: WithIntTerm tm
              => (tm n a -> Int)
              -> tm n a
              -> Maybe Int
termSizeTmMul size =
  fmap termSizeTmMul' .
  preview _TmMul
  where
    termSizeTmMul' (x, y) =
      1 + size x + size y

termSizeTmExp :: WithIntTerm tm
              => (tm n a -> Int)
              -> tm n a
              -> Maybe Int
termSizeTmExp size =
  fmap termSizeTmExp' .
  preview _TmExp
  where
    termSizeTmExp' (x, y) =
      1 + size x + size y

termSizeInput :: WithIntTerm tm
              => TermSizeInput tm n a
termSizeInput =
  TermSizeInput
    [ TermSizeBase termSizeTmInt
    , TermSizeRecurse termSizeTmAdd
    , TermSizeRecurse termSizeTmSub
    , TermSizeRecurse termSizeTmMul
    , TermSizeRecurse termSizeTmExp
    ]
