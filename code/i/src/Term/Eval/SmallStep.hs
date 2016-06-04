{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Small-step rules and helpers for the I language.
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

-- | The first small-step congruence rule for addition.
eAdd1 :: (Term -> Maybe Term) -- ^ The small-step function for the I language.
      -> Term
      -> Maybe Term
eAdd1 step (TmAdd tm1 tm2) =
  TmAdd <$> step tm1 <*> pure tm2
eAdd1 _ _ =
  Nothing

-- | The second small-step congruence rule for addition.
eAdd2 :: (Term -> Maybe Term) -- ^ The small-step function for the I language.
      -> Term
      -> Maybe Term
eAdd2 step (TmAdd tm1 tm2) =
  TmAdd <$> value tm1 <*> step tm2
eAdd2 _ _ =
  Nothing

-- | The small-step reduction rule for addition.
eAddIntInt :: Term
           -> Maybe Term
eAddIntInt (TmAdd (TmInt i1) (TmInt i2)) =
  Just (TmInt (i1 + i2))
eAddIntInt _ =
  Nothing

-- | The first small-step congruence rule for subtraction.
eSub1 :: (Term -> Maybe Term) -- ^ The small-step function for the I language.
      -> Term
      -> Maybe Term
eSub1 step (TmSub tm1 tm2) =
  TmSub <$> step tm1 <*> pure tm2
eSub1 _ _ =
  Nothing

-- | The second small-step congruence rule for subtraction.
eSub2 :: (Term -> Maybe Term) -- ^ The small-step function for the I language.
      -> Term
      -> Maybe Term
eSub2 step (TmSub tm1 tm2) =
  TmSub <$> value tm1 <*> step tm2
eSub2 _ _ =
  Nothing

-- | The small-step reduction rule for subtraction.
eSubIntInt :: Term
           -> Maybe Term
eSubIntInt (TmSub (TmInt i1) (TmInt i2)) =
  Just (TmInt (i1 - i2))
eSubIntInt _ =
  Nothing

-- | The first small-step congruence rule for multiplication.
eMul1 :: (Term -> Maybe Term) -- ^ The small-step function for the I language.
      -> Term
      -> Maybe Term
eMul1 step (TmMul tm1 tm2) =
  TmMul <$> step tm1 <*> pure tm2
eMul1 _ _ =
  Nothing

-- | The second small-step congruence rule for multiplication.
eMul2 :: (Term -> Maybe Term) -- ^ The small-step function for the I language.
      -> Term
      -> Maybe Term
eMul2 step (TmMul tm1 tm2) =
  TmMul <$> value tm1 <*> step tm2
eMul2 _ _ =
  Nothing

-- | The small-step reduction rule for multiplication.
eMulIntInt :: Term
           -> Maybe Term
eMulIntInt (TmMul (TmInt i1) (TmInt i2)) =
  Just (TmInt (i1 * i2))
eMulIntInt _ =
  Nothing

-- | The first small-step congruence rule for exponentiation.
eExp1 :: (Term -> Maybe Term) -- ^ The small-step function for the I language.
      -> Term
      -> Maybe Term
eExp1 step (TmExp tm1 tm2) =
  TmExp <$> step tm1 <*> pure tm2
eExp1 _ _ =
  Nothing

-- | The second small-step congruence rule for exponentiation.
eExp2 :: (Term -> Maybe Term) -- ^ The small-step function for the I language.
      -> Term
      -> Maybe Term
eExp2 step (TmExp tm1 tm2) =
  TmExp <$> value tm1 <*> step tm2
eExp2 _ _ =
  Nothing

-- | The small-step reduction rule for exponentiation.
eExpIntInt :: Term
           -> Maybe Term
eExpIntInt (TmExp (TmInt i1) (TmInt i2)) =
  Just . TmInt $
    -- this is bad, and we should feel bad for doing it
    if i2 < 0 then 0 else i1 ^ i2
eExpIntInt _ =
  Nothing

-- | The set of small-step rules for the I language.
--
-- The small-step rules return a 'Just' value when their
-- input can take a step, and a 'Nothing' value otherwise.
smallStepRules :: [Term -> Maybe Term]
smallStepRules =
  [ eAdd1 smallStep
  , eAdd2 smallStep
  , eAddIntInt
  , eSub1 smallStep
  , eSub2 smallStep
  , eSubIntInt
  , eMul1 smallStep
  , eMul2 smallStep
  , eMulIntInt
  , eExp1 smallStep
  , eExp2 smallStep
  , eExpIntInt
  ]

-- | The small-step function for the I language.
--
-- The small-step function returns a 'Just' value when
-- the given term can take a step, and returns 'Nothing'
-- otherwise.
--
-- This function is built from the contents of 'smallStepRules'.
--
-- >>> smallStep (TmInt 3)
-- Nothing
--
-- >>> smallStep (TmAdd (TmInt 2) (TmInt 5))
-- Just (TmInt 7)
--
-- >>> smallStep (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- Just (TmMul (TmInt 3) (TmInt 7))
smallStep :: Term
          -> Maybe Term
smallStep tm =
  asum .
  fmap ($ tm) $
  smallStepRules

-- | Evaluates a term in the I language.
--
-- The evaluation will continue for as long as there are small-step rules that apply.
-- Evaluation of finite terms is guaranteed to terminates for the I language.
--
-- This evaluation function is implemented in terms of the 'smallStep' function, and
-- consequently relies on 'smallStepRules'.
--
-- >>> eval (TmInt 3)
-- TmInt 3
--
-- >>> eval (TmAdd (TmInt 2) (TmInt 5))
-- TmInt 7
--
-- >>> eval (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- TmInt 21
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
-- >>> isNormalForm (TmInt 3)
-- True
--
-- >>> isNormalForm (TmAdd (TmInt 2) (TmInt 5))
-- False
--
-- >>> isNormalForm (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- False
isNormalForm :: Term
             -> Bool
isNormalForm =
  isNothing .
  smallStep

-- | Determines whether a given term can take a step.
--
-- >>> canStep (TmInt 3)
-- False
--
-- >>> canStep (TmAdd (TmInt 2) (TmInt 5))
-- True
--
-- >>> canStep (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- True
canStep :: Term
        -> Bool
canStep =
  isJust .
  smallStep
