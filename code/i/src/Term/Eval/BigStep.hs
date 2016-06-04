{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : POSIX

Big step rules and helpers for the I language.
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

-- | The big-step rule for 'TmInt'.
eInt :: Term
     -> Maybe Term
eInt (TmInt i) =
  Just (TmInt i)
eInt _ =
  Nothing

intLit :: Term -> Maybe Int
intLit (TmInt i) =
  Just i
intLit _ =
  Nothing

-- | The big-step rule for addition.
eAdd :: (Term -> Maybe Term) -- ^ The big-step function for the I language.
     -> Term
     -> Maybe Term
eAdd step (TmAdd tm1 tm2) = do
  tm1' <- step tm1
  i1 <- intLit tm1'
  tm2' <- step tm2
  i2 <- intLit tm2'
  return $ TmInt (i1 + i2)
eAdd _ _ =
  Nothing

-- | The big-step rule for subtraction.
eSub :: (Term -> Maybe Term) -- ^ The big-step function for the I language.
     -> Term
     -> Maybe Term
eSub step (TmSub tm1 tm2) = do
  tm1' <- step tm1
  i1 <- intLit tm1'
  tm2' <- step tm2
  i2 <- intLit tm2'
  return $ TmInt (i1 - i2)
eSub _ _ =
  Nothing

-- | The big-step rule for multiplication.
eMul :: (Term -> Maybe Term) -- ^ The big-step function for the I language.
     -> Term
     -> Maybe Term
eMul step (TmMul tm1 tm2) = do
  tm1' <- step tm1
  i1 <- intLit tm1'
  tm2' <- step tm2
  i2 <- intLit tm2'
  return $ TmInt (i1 * i2)
eMul _ _ =
  Nothing

-- | The big-step rule for exponentiation.
eExp :: (Term -> Maybe Term) -- ^ The big-step function for the I language.
     -> Term
     -> Maybe Term
eExp step (TmExp tm1 tm2) = do
  tm1' <- step tm1
  i1 <- intLit tm1'
  tm2' <- step tm2
  i2 <- intLit tm2'
  return . TmInt $
    -- this is bad, and we should feel bad for doing it
    if i2 < 0 then 0 else i1 ^ i2
eExp _ _ =
  Nothing

-- | The set of big-step rules for the I language.
--
-- The big-step rules return a 'Just' value when their
-- input can take a step, and a 'Nothing' value otherwise.
bigStepRules :: [Term -> Maybe Term]
bigStepRules =
  [ eInt
  , eAdd bigStep
  , eSub bigStep
  , eMul bigStep
  , eExp bigStep
  ]

-- | The big-step function for the I language.
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
-- >>> bigStep (TmInt 3)
-- Just (TmInt 3)
--
-- >>> bigStep (TmAdd (TmInt 2) (TmInt 5))
-- Just (TmInt 7)
--
-- >>> bigStep (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- Just (TmInt 21)
bigStep :: Term
        -> Maybe Term
bigStep tm =
  asum .
  fmap ($ tm) $
  bigStepRules

-- | Evaluates a term in the I language.
--
-- The evaluation will continue for as long as there are small-step rules that apply.
-- Evaluation of finite terms is guaranteed to terminates for the I language.
--
-- This evaluation function is implemented in terms of the 'bigStep' function, and
-- consequently relies on 'bigStepRules'.
--
-- If 'bigStep' returns 'Nothing', this function will return its input unchanged.
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
  fromMaybe tm .
  bigStep $
  tm
