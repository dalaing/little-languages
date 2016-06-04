{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Small-step rules and helpers for the NB language.

This is the lazy evaluation variant.
-}
module Term.Eval.SmallStep.Lazy (
    smallStepRules
  , smallStep
  , eval
  , isNormalForm
  , canStep
  ) where

-- from 'base'
import           Data.Foldable   (asum)
import           Data.Maybe      (isJust, isNothing)

-- local
import           Term            (Term (..))

-- | The small-step rule for the 'pred O' case.
ePredZero :: Term
          -> Maybe Term
ePredZero (TmPred TmZero) =
  Just TmZero
ePredZero _ =
  Nothing

-- | The small-step rule for the 'pred S' case.
ePredSucc :: Term
          -> Maybe Term
ePredSucc (TmPred (TmSucc tm)) =
  Just tm
ePredSucc _ =
  Nothing

-- | The small-step rule for the 'pred tm' where tm is not a value.
ePred :: (Term -> Maybe Term) -- ^ The small-step function for NB
      -> Term
      -> Maybe Term
ePred step (TmPred tm) = do
  tm' <- step tm
  return $ TmPred tm'
ePred _ _ =
  Nothing

-- | The small-step rule for the 'if true' case.
eIfTrue :: Term
        -> Maybe Term
eIfTrue (TmIf TmTrue t2 _) =
  Just t2
eIfTrue _ =
  Nothing

-- | The small-step rule for the 'if false' case.
eIfFalse :: Term
         -> Maybe Term
eIfFalse (TmIf TmFalse _ t3) =
  Just t3
eIfFalse _ =
  Nothing

-- | The small-step rule for an 'if' expression where the
-- condition is not a value.
eIf :: (Term -> Maybe Term) -- ^ The small-step function for the NB language.
    -> Term
    -> Maybe Term
eIf step (TmIf tm1 tm2 tm3) = do
  tm1' <- step tm1
  return $ TmIf tm1' tm2 tm3
eIf _ _ =
  Nothing

-- | The small-step rule for the 'isZero O' case.
eIsZeroZero :: Term
            -> Maybe Term
eIsZeroZero (TmIsZero TmZero) =
  Just TmTrue
eIsZeroZero _ =
  Nothing

-- | The small-step rule for the 'isZero S' case.
eIsZeroSucc :: Term
            -> Maybe Term
eIsZeroSucc (TmIsZero (TmSucc _)) =
  Just TmFalse
eIsZeroSucc _ =
  Nothing

-- | The small-step rule for the 'isZero tm' where tm is not a value.
eIsZero :: (Term -> Maybe Term) -- ^ The small-step function for NB
        -> Term
        -> Maybe Term
eIsZero step (TmIsZero tm) = do
  tm' <- step tm
  return $ TmIsZero tm'
eIsZero _ _ =
  Nothing

-- | The set of small-step rules for the NB language.
--
-- The small-step rules return a 'Just' value when their
-- input can take a step, and a 'Nothing' value otherwise.
smallStepRules :: [Term -> Maybe Term]
smallStepRules =
  [ ePredZero
  , ePredSucc
  , ePred smallStep
  , eIfTrue
  , eIfFalse
  , eIf smallStep
  , eIsZeroZero
  , eIsZeroSucc
  , eIsZero smallStep
  ]

-- | The small-step function for the NB language.
--
-- The small-step function returns a 'Just' value when
-- the given term can take a step, and returns 'Nothing'
-- otherwise.
--
-- This function is built from the contents of 'smallStepRules'.
--
-- >>> smallStep TmZero
-- Nothing
--
-- >>> smallStep (TmSucc TmZero)
-- Nothing
--
-- >>> smallStep (TmPred TmZero)
-- Just TmZero
--
-- >>> smallStep (TmPred (TmSucc TmZero))
-- Just TmZero
--
-- >>> smallStep (TmSucc (TmPred TmZero))
-- Nothing
--
-- >>> smallStep (TmPred (TmSucc (TmPred TmZero)))
-- Just (TmPred TmZero)
--
-- >>> smallStep TmFalse
-- Nothing
--
-- >>> smallStep (TmIf TmFalse TmFalse TmTrue)
-- Just TmTrue
--
-- >>> smallStep (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- Just (TmIf TmTrue TmFalse TmTrue)
--
-- >>> smallStep (TmIsZero (TmSucc TmZero))
-- Just TmFalse
--
-- >>> smallStep (TmIsZero (TmPred (TmSucc TmZero)))
-- Just (TmIsZero TmZero)
smallStep :: Term
          -> Maybe Term
smallStep tm =
  asum .
  fmap ($ tm) $
  smallStepRules

-- | Evaluates a term in the NB language.
--
-- The evaluation will continue for as long as there are small-step rules that apply.
-- Evaluation of finite terms is guaranteed to terminates for the NB language.
--
-- This evaluation function is implemented in terms of the 'smallStep' function, and
-- consequently relies on 'smallStepRules'.
--
-- >>> eval TmZero
-- TmZero
--
-- >>> eval (TmSucc TmZero)
-- TmSucc TmZero
--
-- >>> eval (TmPred TmZero)
-- TmZero
--
-- >>> eval (TmPred (TmSucc TmZero))
-- TmZero
--
-- >>> eval (TmSucc (TmPred TmZero))
-- TmSucc (TmPred TmZero)
--
-- >>> eval (TmPred (TmSucc (TmPred TmZero)))
-- TmZero
--
-- >>> eval TmFalse
-- TmFalse
--
-- >>> eval (TmIf TmFalse TmFalse TmTrue)
-- TmTrue
--
-- >>> eval (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- TmFalse
--
-- >>> eval (TmIsZero (TmSucc TmZero))
-- TmFalse
--
-- >>> eval (TmIsZero (TmPred (TmSucc TmZero)))
-- TmTrue
eval :: Term
     -> Term
eval tm =
  case smallStep tm of
    Nothing -> tm
    Just tm' -> eval tm'

-- | Determines whether a given term is in normal form.
--
-- A term is in normal form if there are no small-step rules that apply to it.
--
-- >>> isNormalForm TmZero
-- True
--
-- >>> isNormalForm (TmPred TmZero)
-- False
--
-- >>> isNormalForm TmFalse
-- True
--
-- >>> isNormalForm (TmIf TmFalse TmFalse TmTrue)
-- False
--
-- >>> isNormalForm (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- False
--
-- >>> isNormalForm (TmIsZero (TmSucc TmZero))
-- False
isNormalForm :: Term
             -> Bool
isNormalForm =
  isNothing .
  smallStep

-- | Determines whether a given term can take a step.
--
-- >>> canStep TmZero
-- False
--
-- >>> canStep (TmPred TmZero)
-- True
--
-- >>> canStep TmFalse
-- False
--
-- >>> canStep (TmIf TmFalse TmFalse TmTrue)
-- True
--
-- >>> canStep (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- True
--
-- >>> canStep (TmIsZero (TmSucc TmZero))
-- True
canStep :: Term
        -> Bool
canStep =
  isJust .
  smallStep
