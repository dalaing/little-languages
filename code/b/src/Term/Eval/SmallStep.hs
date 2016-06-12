{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Small-step rules and helpers for the B language.

<<images/ss-iftrue.png>>

<<images/ss-iffalse.png>>

<<images/ss-if.png>>

-}
module Term.Eval.SmallStep (
    smallStepRules
  , smallStep
  , eval
  , isNormalForm
  , canStep
  ) where

-- from 'base'
import           Data.Foldable (asum)
import           Data.Maybe    (isJust, isNothing)

-- local
import           Term          (Term (..))

-- | The small-step rule for the 'if true' case.
--
-- <<images/ss-iftrue.png>>
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
eIf :: (Term -> Maybe Term) -- ^ The small-step function for the B language.
    -> Term
    -> Maybe Term
eIf step (TmIf tm1 tm2 tm3) = do
  tm1' <- step tm1
  return $ TmIf tm1' tm2 tm3
eIf _ _ =
  Nothing

-- | The set of small-step rules for the B language.
--
-- The small-step rules return a 'Just' value when their
-- input can take a step, and a 'Nothing' value otherwise.
smallStepRules :: [Term -> Maybe Term]
smallStepRules =
  [ eIfTrue
  , eIfFalse
  , eIf smallStep
  ]

-- | The small-step function for the B language.
--
-- The small-step function returns a 'Just' value when
-- the given term can take a step, and returns 'Nothing'
-- otherwise.
--
-- This function is built from the contents of 'smallStepRules'.
--
-- >>> smallStep TmFalse
-- Nothing
--
-- >>> smallStep (TmIf TmFalse TmFalse TmTrue)
-- Just TmTrue
--
-- >>> smallStep (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- Just (TmIf TmTrue TmFalse TmTrue)
smallStep :: Term
          -> Maybe Term
smallStep tm =
  asum .
  fmap ($ tm) $
  smallStepRules

-- | Evaluates a term in the B language.
--
-- The evaluation will continue for as long as there are small-step rules that apply.
-- Evaluation of finite terms is guaranteed to terminates for the B language.
--
-- This evaluation function is implemented in terms of the 'smallStep' function, and
-- consequently relies on 'smallStepRules'.
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
  case smallStep tm of
    Nothing -> tm
    Just tm' -> eval tm'

-- | Determines whether a given term is in normal form.
--
-- A term is in normal form if there are no small-step rules that apply to it.
--
-- >>> isNormalForm TmFalse
-- True
--
-- >>> isNormalForm (TmIf TmFalse TmFalse TmTrue)
-- False
--
-- >>> isNormalForm (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- False
isNormalForm :: Term
             -> Bool
isNormalForm =
  isNothing .
  smallStep

-- | Determines whether a given term can take a step.
--
-- >>> canStep TmFalse
-- False
--
-- >>> canStep (TmIf TmFalse TmFalse TmTrue)
-- True
--
-- >>> canStep (TmIf (TmIf TmFalse TmFalse TmTrue) TmFalse TmTrue)
-- True
canStep :: Term
        -> Bool
canStep =
  isJust .
  smallStep
