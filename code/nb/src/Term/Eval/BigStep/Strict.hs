{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Big-step rules and helpers for the NB language.

This is the strict evaluation variant.
-}
module Term.Eval.BigStep.Strict (
    bigStepRules
  , bigStep
  , eval
  ) where

-- from 'base'
import           Data.Foldable (asum)
import           Data.Maybe    (fromMaybe)

-- local
import           Term          (Term (..))

-- | The big-step rule for 'TmZero'.
eZero :: Term
      -> Maybe Term
eZero TmZero =
  Just TmZero
eZero _ =
  Nothing

-- | The big-step rule for 'TmSucc'.
eSucc :: (Term -> Maybe Term) -- ^ The big-step function for the N language.
      -> Term
      -> Maybe Term
eSucc step (TmSucc tm) =
  TmSucc <$> step tm
eSucc _ _ =
  Nothing

-- | The big-step rule for 'pred O'.
ePredZero :: (Term -> Maybe Term) -- ^ The big-step function for the N language.
      -> Term
      -> Maybe Term
ePredZero step (TmPred tm) =
  case step tm of
    Just TmZero -> Just TmZero
    _ -> Nothing
ePredZero _ _ =
  Nothing

-- | The big-step rule for 'pred S'.
ePredSucc :: (Term -> Maybe Term) -- ^ The big-step function for the N language.
      -> Term
      -> Maybe Term
ePredSucc step (TmPred tm) =
  case step tm of
    Just (TmSucc tm') -> Just tm'
    _ -> Nothing
ePredSucc _ _ =
  Nothing

-- | The big-step rule for 'TmTrue'.
eTrue :: Term
      -> Maybe Term
eTrue TmTrue =
  Just TmTrue
eTrue _ =
  Nothing

-- | The big-step rule for 'TmFalse'.
eFalse :: Term
       -> Maybe Term
eFalse TmFalse =
  Just TmFalse
eFalse _ =
  Nothing

-- | The big-step rule for the 'if-true' case.
eIfTrue :: (Term -> Maybe Term) -- ^ The big-step function for the NB language.
        -> Term
        -> Maybe Term
eIfTrue step (TmIf t1 t2 _)
  | step t1 == Just TmTrue =
    step t2
  | otherwise =
    Nothing
eIfTrue _ _ =
  Nothing

-- | The big-step rule for the 'if-false' case.
eIfFalse :: (Term -> Maybe Term) -- ^ The big-step function for the NB language.
         -> Term
         -> Maybe Term
eIfFalse step (TmIf t1 _ t3)
  | step t1 == Just TmFalse =
    step t3
  | otherwise =
    Nothing
eIfFalse _ _ =
  Nothing

-- | The big-step rule for the 'isZero O' case.
eIsZeroZero :: (Term -> Maybe Term) -- ^ The big-step function for the NB language.
            -> Term
            -> Maybe Term
eIsZeroZero step (TmIsZero tm) =
  case step tm of
    Just TmZero -> Just TmTrue
    _ -> Nothing
eIsZeroZero _ _ =
  Nothing

-- | The big-step rule for the 'isZero S' case.
eIsZeroSucc :: (Term -> Maybe Term) -- ^ The big-step function for the NB language.
            -> Term
            -> Maybe Term
eIsZeroSucc step (TmIsZero tm) =
  case step tm of
    Just (TmSucc _) -> Just TmFalse
    _ -> Nothing
eIsZeroSucc _ _ =
  Nothing

-- | The set of big-step rules for the NB language.
--
-- The big-step rules return a 'Just' value when their
-- input can take a step, and a 'Nothing' value otherwise.
bigStepRules :: [Term -> Maybe Term]
bigStepRules =
  [ eZero
  , eSucc bigStep
  , ePredZero bigStep
  , ePredSucc bigStep
  , eTrue
  , eFalse
  , eIfTrue bigStep
  , eIfFalse bigStep
  , eIsZeroZero bigStep
  , eIsZeroSucc bigStep
  ]

-- | The big-step function for the NB language.
--
-- The big-step function returns a 'Just' value when
-- the given term can take a step, and returns 'Nothing'
-- otherwise.
--
-- This function is built from the contents of 'bigStepRules'.
--
-- The big-step rules should be exhaustive, and so
-- 'bigStep' should never actually return 'Nothing'.
--
-- >>> bigStep TmZero
-- Just TmZero
--
-- >>> bigStep (TmSucc TmZero)
-- Just (TmSucc TmZero)
--
-- >>> bigStep (TmPred TmZero)
-- Just TmZero
--
-- >>> bigStep (TmPred (TmSucc TmZero))
-- Just TmZero
--
-- >>> bigStep (TmSucc (TmPred TmZero))
-- Just (TmSucc TmZero)
--
-- >>> bigStep (TmPred (TmSucc (TmPred TmZero)))
-- Just TmZero
--
-- >>> bigStep TmFalse
-- Just TmFalse
--
-- >>> bigStep (TmIf TmFalse TmFalse TmTrue)
-- Just TmTrue
--
-- >>> bigStep (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- Just TmFalse
--
-- >>> bigStep (TmIsZero (TmSucc TmZero))
-- Just TmFalse
--
-- >>> bigStep (TmIsZero (TmPred (TmSucc TmZero)))
-- Just TmTrue
bigStep :: Term
        -> Maybe Term
bigStep tm =
  asum .
  fmap ($ tm) $
  bigStepRules

-- | Evaluates a term in the N language.
--
-- The evaluation will continue for as long as there are small-step rules that apply.
-- Evaluation of finite terms is guaranteed to terminates for the N language.
--
-- This evaluation function is implemented in terms of the 'bigStep' function, and
-- consequently relies on 'bigStepRules'.
--
-- If 'bigStep' returns 'Nothing', this function will return its input unchanged.
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
  fromMaybe tm .
  bigStep $
  tm
