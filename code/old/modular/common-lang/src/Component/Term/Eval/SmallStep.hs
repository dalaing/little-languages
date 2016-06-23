{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Component.Term.Eval.SmallStep (
    SmallStepRule(..)
  , SmallStepInput(..)
  , SmallStepOutput(..)
  , HasSmallStepOutput(..)
  , mkSmallStep
  ) where

import Data.Maybe (isJust, isNothing)
import Data.Foldable (asum)

import Control.Lens.TH (makeClassy)

import Component.Term.Eval.Value (ValueOutput(..))
import Component.Term.Note.Strip (StripNoteTerm(..))

-- |
data SmallStepRule tm =
    SmallStepBase (forall nTy nTm. tm nTy nTm String -> Maybe (tm nTy nTm String))                        -- ^
  | SmallStepValue (forall nTy nTm. (tm nTy nTm String -> Maybe (tm nTy nTm String)) -> tm nTy nTm String -> Maybe (tm nTy nTm String)) -- ^
  | SmallStepRecurse (forall nTy nTm. (tm nTy nTm String -> Maybe (tm nTy nTm String)) -> tm nTy nTm String -> Maybe (tm nTy nTm String)) -- ^
  | SmallStepValueRecurse (forall nTy nTm. (tm nTy nTm String -> Maybe (tm nTy nTm String)) -> (tm nTy nTm String -> Maybe (tm nTy nTm String)) -> tm nTy nTm String -> Maybe (tm nTy nTm String)) -- ^

-- |
fixSmallStepRule :: (tm nTy nTm String -> Maybe (tm nTy nTm String))
                 -> (tm nTy nTm String -> Maybe (tm nTy nTm String))
                 -> SmallStepRule tm
                 -> tm nTy nTm String
                 -> Maybe (tm nTy nTm String)
fixSmallStepRule _ _ (SmallStepBase f) x =
  f x
fixSmallStepRule value _ (SmallStepValue f) x =
  f value x
fixSmallStepRule _ step (SmallStepRecurse f) x =
  f step x
fixSmallStepRule value step (SmallStepValueRecurse f) x =
  f value step x

-- |
data SmallStepInput tm =
  SmallStepInput [SmallStepRule tm] -- ^

instance Monoid (SmallStepInput tm) where
  mempty =
    SmallStepInput mempty
  mappend (SmallStepInput v1) (SmallStepInput v2) =
    SmallStepInput (mappend v1 v2)

-- |
data SmallStepOutput tm =
  SmallStepOutput {
    _smallStep      :: forall nTy nTm. tm nTy nTm String -> Maybe (tm nTy nTm String)   -- ^
  , _smallStepRules :: forall nTy nTm. [tm nTy nTm String -> Maybe (tm nTy nTm String)] -- ^
  , _smallStepEval  :: forall nTy nTm. tm nTy nTm String -> tm nTy nTm String  -- ^
  , _canStep        :: forall nTy nTm. tm nTy nTm String -> Bool -- ^
  , _isNormalForm   :: forall nTy nTm. tm nTy nTm String -> Bool -- ^
  }

makeClassy ''SmallStepOutput

-- |
mkSmallStep :: StripNoteTerm tm tm
            => ValueOutput tm
            -> SmallStepInput tm -- ^
            -> SmallStepOutput tm -- ^
mkSmallStep v (SmallStepInput i) =
  let
    smallStepRules' =
      fmap (fixSmallStepRule (_value v) smallStep') i
    smallStep' tm =
      asum .
      fmap ($ stripNoteTerm tm) $
      smallStepRules'
    smallStepEval' tm =
      case smallStep' tm of
        Nothing -> stripNoteTerm tm
        Just tm' -> smallStepEval' tm'
  in
    SmallStepOutput
      smallStep'
      smallStepRules'
      smallStepEval'
      (isJust . smallStep')
      (isNothing . smallStep')
