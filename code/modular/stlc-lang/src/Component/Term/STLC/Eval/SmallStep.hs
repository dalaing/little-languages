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
import Data.Constraint.Forall (ForallT)

import Component.Term.Eval.SmallStep (SmallStepRule(..), SmallStepInput(..))

import Component.Term.STLC (AsSTLCTerm(..), WithSTLCTerm, app_)

eApp1 :: WithSTLCTerm tm ty
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a
      -> Maybe (tm nTy nTm a)
eApp1 smallStep tm = do
  (tm1, tm2) <- preview _TmApp tm
  tm1' <- smallStep tm1
  return $ review _TmApp (tm1', tm2)

eApp2 :: WithSTLCTerm tm ty
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a
      -> Maybe (tm nTy nTm a)
eApp2 value smallStep tm = do
  (tm1, tm2) <- preview _TmApp tm
  _ <- value tm1
  tm2' <- smallStep tm2
  return $ review _TmApp (tm1, tm2')

eAppLam :: ( WithSTLCTerm tm ty
           , ForallT Monad tm
           )
        => tm nTy nTm String
        -> Maybe (tm nTy nTm String)
eAppLam tm = do
  (tm1, tm2) <- preview _TmApp tm
  (_, _, s) <- preview _TmLam tm1
  return $ app_ tm2 s

smallStepInput :: ( WithSTLCTerm tm ty
                  , ForallT Monad tm
                  )
               => SmallStepInput tm nTy nTm String
smallStepInput =
  SmallStepInput
   [ SmallStepBase eAppLam
   , SmallStepRecurse eApp1
   , SmallStepValueRecurse eApp2
   ]
