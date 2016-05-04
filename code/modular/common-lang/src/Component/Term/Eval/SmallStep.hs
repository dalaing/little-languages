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
data SmallStepRule tm nTy nTm a =
    SmallStepBase (tm nTy nTm a -> Maybe (tm nTy nTm a))                        -- ^
  | SmallStepValue ((tm nTy nTm a -> Maybe (tm nTy nTm a)) -> tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
  | SmallStepRecurse ((tm nTy nTm a -> Maybe (tm nTy nTm a)) -> tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
  | SmallStepValueRecurse ((tm nTy nTm a -> Maybe (tm nTy nTm a)) -> (tm nTy nTm a -> Maybe (tm nTy nTm a)) -> tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^

-- |
fixSmallStepRule :: (tm nTy nTm a -> Maybe (tm nTy nTm a))
                 -> (tm nTy nTm a -> Maybe (tm nTy nTm a))
                 -> SmallStepRule tm nTy nTm a
                 -> tm nTy nTm a
                 -> Maybe (tm nTy nTm a)
fixSmallStepRule _ _ (SmallStepBase f) x =
  f x
fixSmallStepRule value _ (SmallStepValue f) x =
  f value x
fixSmallStepRule _ step (SmallStepRecurse f) x =
  f step x
fixSmallStepRule value step (SmallStepValueRecurse f) x =
  f value step x

-- |
data SmallStepInput tm nTy nTm a =
  SmallStepInput [SmallStepRule tm nTy nTm a] -- ^

instance Monoid (SmallStepInput tm nTy nTm a) where
  mempty =
    SmallStepInput mempty
  mappend (SmallStepInput v1) (SmallStepInput v2) =
    SmallStepInput (mappend v1 v2)

-- |
data SmallStepOutput tm nTy nTm a =
  SmallStepOutput {
    _smallStep      :: tm nTy nTm a -> Maybe (tm nTy nTm a)   -- ^
  , _smallStepRules :: [tm nTy nTm a -> Maybe (tm nTy nTm a)] -- ^
  , _smallStepEval  :: tm nTy nTm a -> tm nTy nTm a  -- ^
  , _canStep        :: tm nTy nTm a -> Bool -- ^
  , _isNormalForm   :: tm nTy nTm a -> Bool -- ^
  }

makeClassy ''SmallStepOutput

-- |
mkSmallStep :: StripNoteTerm tm tm
            => ValueOutput tm nTy nTm a
            -> SmallStepInput tm nTy nTm a  -- ^
            -> SmallStepOutput tm nTy nTm a -- ^
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
