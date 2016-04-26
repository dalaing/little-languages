{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.STLC.Eval.SmallStep (
    smallStepInput
  ) where

import Control.Lens (preview, review)
import Bound (instantiate1)

import Component.Term.Eval.SmallStep (SmallStepRule(..), SmallStepInput(..))

import Component.Term.STLC (AsSTLCTerm(..), WithSTLCTerm)

eApp1 :: WithSTLCTerm tm ty nTy nTm a
      => (tm nTm a -> Maybe (tm nTm a))
      -> tm nTm a
      -> Maybe (tm nTm a)
eApp1 smallStep tm = do
  (tm1, tm2) <- preview _TmApp tm
  tm1' <- smallStep tm1
  return $ review _TmApp (tm1', tm2)

eApp2 :: WithSTLCTerm tm ty nTy nTm a
      => (tm nTm a -> Maybe (tm nTm a))
      -> (tm nTm a -> Maybe (tm nTm a))
      -> tm nTm a
      -> Maybe (tm nTm a)
eApp2 value smallStep tm = do
  (tm1, tm2) <- preview _TmApp tm
  _ <- value tm1
  tm2' <- smallStep tm2
  return $ review _TmApp (tm1, tm2')

eAppLam :: ( WithSTLCTerm tm ty nTy nTm a
           , Monad (tm nTm)
           )
        => tm nTm a
        -> Maybe (tm nTm a)
eAppLam tm = do
  (tm1, tm2) <- preview _TmApp tm
  (_, _, s) <- preview _TmLam tm1
  return $ instantiate1 tm2 s

smallStepInput :: ( WithSTLCTerm tm ty nTy nTm a
                  , Monad (tm nTm)
                  )
               => SmallStepInput tm nTm a
smallStepInput =
  SmallStepInput
   [ SmallStepBase eAppLam
   , SmallStepRecurse eApp1
   , SmallStepValueRecurse eApp2
   ]
