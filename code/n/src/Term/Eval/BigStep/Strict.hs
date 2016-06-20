{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Big-step rules and helpers for the N language.

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

-- | The set of big-step rules for the N language.
--
-- The big-step rules return a 'Just' value when their
-- input can take a step, and a 'Nothing' value otherwise.
bigStepRules :: [Term -> Maybe Term]
bigStepRules =
  [ eZero
  , eSucc bigStep
  , ePredZero bigStep
  , ePredSucc bigStep
  ]

-- | The big-step function for the N language.
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
eval :: Term
     -> Term
eval tm =
  fromMaybe tm .
  bigStep $
  tm
