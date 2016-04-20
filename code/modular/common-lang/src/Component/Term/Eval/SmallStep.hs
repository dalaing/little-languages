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
import Component.Term.Note.Strip (StripNoteTermOutput(..))

-- |
data SmallStepRule tm n a =
    SmallStepBase (tm n a -> Maybe (tm n a))                        -- ^
  | SmallStepValue ((tm n a -> Maybe (tm n a)) -> tm n a -> Maybe (tm n a)) -- ^
  | SmallStepRecurse ((tm n a -> Maybe (tm n a)) -> tm n a -> Maybe (tm n a)) -- ^
  | SmallStepValueRecurse ((tm n a -> Maybe (tm n a)) -> (tm n a -> Maybe (tm n a)) -> tm n a -> Maybe (tm n a)) -- ^

-- |
fixSmallStepRule :: (tm n a -> Maybe (tm n a))
                 -> (tm n a -> Maybe (tm n a))
                 -> SmallStepRule tm n a
                 -> tm n a
                 -> Maybe (tm n a)
fixSmallStepRule _ _ (SmallStepBase f) x =
  f x
fixSmallStepRule value _ (SmallStepValue f) x =
  f value x
fixSmallStepRule _ step (SmallStepRecurse f) x =
  f step x
fixSmallStepRule value step (SmallStepValueRecurse f) x =
  f value step x

-- |
data SmallStepInput tm n a =
  SmallStepInput [SmallStepRule tm n a] -- ^

instance Monoid (SmallStepInput tm n a) where
  mempty =
    SmallStepInput mempty
  mappend (SmallStepInput v1) (SmallStepInput v2) =
    SmallStepInput (mappend v1 v2)

-- |
data SmallStepOutput tm n a =
  SmallStepOutput {
    _smallStep      :: tm n a -> Maybe (tm n a)   -- ^
  , _smallStepRules :: [tm n a -> Maybe (tm n a)] -- ^
  , _smallStepEval  :: tm n a -> tm n a  -- ^
  , _canStep        :: tm n a -> Bool -- ^
  , _isNormalForm   :: tm n a -> Bool -- ^
  }

makeClassy ''SmallStepOutput

-- |
mkSmallStep :: StripNoteTermOutput tm
            -> ValueOutput tm n a
            -> SmallStepInput tm n a  -- ^
            -> SmallStepOutput tm n a -- ^
mkSmallStep (StripNoteTermOutput _ stripNote) v (SmallStepInput i) =
  let
    smallStepRules' =
      fmap (fixSmallStepRule (_value v) smallStep') i
    smallStep' tm =
      asum .
      fmap ($ stripNote tm) $
      smallStepRules'
    smallStepEval' tm =
      case smallStep' tm of
        Nothing -> stripNote tm
        Just tm' -> smallStepEval' tm'
  in
    SmallStepOutput
      smallStep'
      smallStepRules'
      smallStepEval'
      (isJust . smallStep')
      (isNothing . smallStep')
