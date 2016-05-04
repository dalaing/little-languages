{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Int.SubTerm (
    subTermInput
  ) where

import Control.Lens (preview, review)

import Component.Term.SubTerm (SubTermInput(..), SubTermRule(..))

import Component.Term.Int (AsIntTerm(..), WithIntTerm)

subTermTmInt :: WithIntTerm tm
             => tm nTy nTm a
             -> Maybe [tm nTy nTm a]
subTermTmInt =
  fmap (pure . review _TmIntLit) .
  preview _TmIntLit

subTermTmAdd :: WithIntTerm tm
             => (tm nTy nTm a -> [tm nTy nTm a])
             -> tm nTy nTm a
             -> Maybe [tm nTy nTm a]
subTermTmAdd subTerms tm =
    fmap subTermTmAdd' .
    preview _TmAdd $
    tm
  where
    subTermTmAdd' (x, y) =
      tm : subTerms x ++ subTerms y

subTermTmSub :: WithIntTerm tm
             => (tm nTy nTm a -> [tm nTy nTm a])
             -> tm nTy nTm a
             -> Maybe [tm nTy nTm a]
subTermTmSub subTerm tm =
    fmap subTermTmSub' .
    preview _TmSub $
    tm
  where
    subTermTmSub' (x, y) =
      tm : subTerm x ++ subTerm y

subTermTmMul :: WithIntTerm tm
              => (tm nTy nTm a -> [tm nTy nTm a])
              -> tm nTy nTm a
              -> Maybe [tm nTy nTm a]
subTermTmMul subTerm tm =
    fmap subTermTmMul' .
    preview _TmMul $
    tm
  where
    subTermTmMul' (x, y) =
      tm : subTerm x ++ subTerm y

subTermTmExp :: WithIntTerm tm
             => (tm nTy nTm a -> [tm nTy nTm a])
             -> tm nTy nTm a
             -> Maybe [tm nTy nTm a]
subTermTmExp subTerm tm =
    fmap subTermTmExp' .
    preview _TmExp $
    tm
  where
    subTermTmExp' (x, y) =
      tm : subTerm x ++ subTerm y

subTermInput :: WithIntTerm tm
             => SubTermInput tm nTy nTm a
subTermInput =
  SubTermInput
    [ SubTermBase subTermTmInt
    , SubTermRecurse subTermTmAdd
    , SubTermRecurse subTermTmSub
    , SubTermRecurse subTermTmMul
    , SubTermRecurse subTermTmExp
    ]
