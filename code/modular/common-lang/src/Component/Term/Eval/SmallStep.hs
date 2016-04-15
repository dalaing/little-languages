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

-- |
data SmallStepRule tm =
    SmallStepBase (tm -> Maybe tm)                        -- ^
  | SmallStepValue ((tm -> Maybe tm) -> tm -> Maybe tm) -- ^
  | SmallStepRecurse ((tm -> Maybe tm) -> tm -> Maybe tm) -- ^

-- |
fixSmallStepRule :: (tm -> Maybe tm)
                 -> (tm -> Maybe tm)
                 -> SmallStepRule tm
                 -> tm
                 -> Maybe tm
fixSmallStepRule _ _ (SmallStepBase f) x =
  f x
fixSmallStepRule value _ (SmallStepValue f) x =
  f value x
fixSmallStepRule _ step (SmallStepRecurse f) x =
  f step x

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
    _smallStep      :: tm -> Maybe tm   -- ^
  , _smallStepRules :: [tm -> Maybe tm] -- ^
  , _smallStepEval  :: tm -> tm   -- ^
  , _canStep        :: tm -> Bool -- ^
  , _isNormalForm   :: tm -> Bool -- ^
  }

makeClassy ''SmallStepOutput

-- |
mkSmallStep :: ValueOutput tm
            -> SmallStepInput tm  -- ^
            -> SmallStepOutput tm -- ^
mkSmallStep v (SmallStepInput i) =
  let
    smallStepRules' =
      fmap (fixSmallStepRule (_value v) smallStep') i
    smallStep' tm =
      asum .
      fmap ($ tm) $
      smallStepRules'
    smallStepEval' tm =
      case smallStep' tm of
        Nothing -> tm
        Just tm' -> smallStepEval' tm'
  in
    SmallStepOutput
      smallStep'
      smallStepRules'
      smallStepEval'
      (isJust . smallStep')
      (isNothing . smallStep')
