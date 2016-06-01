{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : POSIX

Big step rules and helpers for the B language.
-}
module Term.Eval.BigStep (
    bigStepRules
  , bigStep
  , eval
  ) where

-- from 'base'
import           Data.Foldable (asum)
import           Data.Maybe    (fromMaybe)

-- local
import           Term          (Term (..))

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
eIfTrue :: (Term -> Maybe Term) -- ^ The big-step function for B
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
eIfFalse :: (Term -> Maybe Term) -- ^ The big-step function for B
         -> Term
         -> Maybe Term
eIfFalse step (TmIf t1 _ t3)
  | step t1 == Just TmFalse =
    step t3
  | otherwise =
    Nothing
eIfFalse _ _ =
  Nothing

-- | The set of big-step rules for the B language.
--
-- The big-step rules return a 'Just' value when their
-- input can take a step, and a 'Nothing' value otherwise.
bigStepRules :: [Term -> Maybe Term]
bigStepRules =
  [ eTrue
  , eFalse
  , eIfTrue bigStep
  , eIfFalse bigStep
  ]

-- | The big-step function for the B language.
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
-- >>> bigStep TmFalse
-- Just TmFalse
--
-- >>> bigStep (TmIf TmFalse TmFalse TmTrue)
-- Just TmTrue
--
-- >>> bigStep (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- Just TmFalse
bigStep :: Term
        -> Maybe Term
bigStep tm =
  asum .
  fmap ($ tm) $
  bigStepRules

-- | Evaluates a term in the B language.
--
-- The evaluation will continue for as long as there are small-step rules that apply.
-- Evaluation of finite terms is guaranteed to terminates for the B language.
--
-- This evaluation function is implemented in terms of the 'bigStep' function, and
-- consequently relies on 'bigStepRules'.
--
-- If 'bigStep' returns 'Nothing', this function will return its input unchanged.
--
-- >>> eval TmFalse
-- TmFalse
--
-- >>> eval (TmIf TmFalse TmFalse TmTrue)
-- TmTrue
--
-- >>> eval (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- TmFalse
eval :: Term
     -> Term
eval tm =
  fromMaybe tm .
  bigStep $
  tm
