{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Small-step rules and helpers for the N language.
-}
module Term.Eval.SmallStep (
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
import           Term.Eval.Value (value)

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
  value tm
ePredSucc _ =
  Nothing

-- | The small-step rule for the 'S tm' where tm is not a value.
eSucc :: (Term -> Maybe Term) -- ^ The small-step function for N
      -> Term
      -> Maybe Term
eSucc step (TmSucc tm) = do
  tm' <- step tm
  return $ TmSucc tm'
eSucc _ _ =
  Nothing

-- | The small-step rule for the 'pred tm' where tm is not a value.
ePred :: (Term -> Maybe Term) -- ^ The small-step function for N
      -> Term
      -> Maybe Term
ePred step (TmPred tm) = do
  tm' <- step tm
  return $ TmPred tm'
ePred _ _ =
  Nothing

-- | The set of small-step rules for the N language.
--
-- The small-step rules return a 'Just' value when their
-- input can take a step, and a 'Nothing' value otherwise.
smallStepRules :: [Term -> Maybe Term]
smallStepRules =
  [ ePredZero
  , ePredSucc
  , eSucc smallStep
  , ePred smallStep
  ]

-- | The small-step function for the N language.
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
-- Just (TmSucc TmZero)
--
-- >>> smallStep (TmPred (TmSucc (TmPred TmZero)))
-- Just (TmPred (TmSucc TmZero))
smallStep :: Term
          -> Maybe Term
smallStep tm =
  asum .
  fmap ($ tm) $
  smallStepRules

-- | Evaluates a term in the N language.
--
-- The evaluation will continue for as long as there are small-step rules that apply.
-- Evaluation of finite terms is guaranteed to terminates for the N language.
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
-- TmSucc TmZero
--
-- >>> eval (TmPred (TmSucc (TmPred TmZero)))
-- TmZero
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
canStep :: Term
        -> Bool
canStep =
  isJust .
  smallStep
