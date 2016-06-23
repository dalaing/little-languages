{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators        #-}
module Component.Term.SubTerm (
    SubTermRule(..)
  , SubTermInput(..)
  , SubTermOutput
  , HasSubTermOutput(..)
  , mkSubTerm
  ) where

import           Control.Lens.TH (makeClassy)
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe)

import Data.Constraint

import Extras (Eq3(..))

-- |
data SubTermRule tm =
    SubTermBase (forall nTy nTm. tm nTy nTm String -> Maybe [tm nTy nTm String])                   -- ^
  | SubTermRecurse (forall nTy nTm. (tm nTy nTm String -> [tm nTy nTm String]) -> tm nTy nTm String -> Maybe [tm nTy nTm String]) -- ^

-- |
fixSubTermRule :: (tm nTy nTm String -> [tm nTy nTm String])
                  -> SubTermRule tm
                  -> tm nTy nTm String
                  -> Maybe [tm nTy nTm String]
fixSubTermRule _ (SubTermBase f) x =
  f x
fixSubTermRule step (SubTermRecurse f) x =
  f step x

-- |
data SubTermInput tm =
  SubTermInput [SubTermRule tm] -- ^

instance Monoid (SubTermInput tm) where
  mempty =
    SubTermInput mempty
  mappend (SubTermInput v1) (SubTermInput v2) =
    SubTermInput (mappend v1 v2)

-- |
data SubTermOutput tm =
  SubTermOutput {
    _subTerms ::     forall nTy nTm. tm nTy nTm String -> [tm nTy nTm String]
  , _containsTerm :: forall nTy nTm. (Eq nTy, Eq nTm) => tm nTy nTm String -> tm nTy nTm String -> Bool
  , _termSize ::     forall nTy nTm. tm nTy nTm String -> Int -- ^
  }

makeClassy ''SubTermOutput

-- |
mkSubTerm :: forall tm. Eq3 tm
          => SubTermInput tm -- ^
          -> SubTermOutput tm -- ^
mkSubTerm (SubTermInput i) =
  let
    subTermRules' =
      fmap (fixSubTermRule subTerms') i
    subTerms' tm =
      fromMaybe [] .
      asum .
      fmap ($ tm) $
      subTermRules'
    containsTerm' :: forall nTy nTm. (Eq nTy, Eq nTm) => tm nTy nTm String -> tm nTy nTm String -> Bool
    containsTerm' x y =
      (elem y . subTerms' $ x) \\ (spanEq3 :: (Eq nTy, Eq nTm, Eq String) :- Eq (tm nTy nTm String))
    termSize' = length . subTerms'
  in
    SubTermOutput
      subTerms'
      containsTerm'
      termSize'
