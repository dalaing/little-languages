module Term.Eval.BigStep where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Term
import Term.Eval.Value (value)

eAdd :: (Term -> Maybe Term)
     -> Term
     -> Maybe Term
eAdd step t = do
  (x, y) <- preview _TmAdd t
  x' <- step x
  x'' <- preview _TmInt x'
  y' <- step y
  y'' <- preview _TmInt y'
  return $ TmInt (x'' + y'')

eSub :: (Term -> Maybe Term)
     -> Term
     -> Maybe Term
eSub step t = do
  (x, y) <- preview _TmSub t
  x' <- step x
  x'' <- preview _TmInt x'
  y' <- step y
  y'' <- preview _TmInt y'
  return $ TmInt (x'' - y'')

eMul :: (Term -> Maybe Term)
     -> Term
     -> Maybe Term
eMul step t = do
  (x, y) <- preview _TmMul t
  x' <- step x
  x'' <- preview _TmInt x'
  y' <- step y
  y'' <- preview _TmInt y'
  return $ TmInt (x'' * y'')

eExp :: (Term -> Maybe Term)
     -> Term
     -> Maybe Term
eExp step t = do
  (x, y) <- preview _TmExp t
  x' <- step x
  x'' <- preview _TmInt x'
  y' <- step y
  y'' <- preview _TmInt y'
  if y'' >= 0
  then
    return $ TmInt (x'' ^ y'')
  else
    -- this is bad, and we should feel bad
    return $ TmInt 0

bigSteps :: [Term -> Maybe Term]
bigSteps =
  [ value
  , eAdd bigStep
  , eSub bigStep
  , eMul bigStep
  , eExp bigStep
  ]

bigStep :: Term
        -> Maybe Term
bigStep t =
  asum .
  map ($ t) $
  bigSteps

bEval :: Term
      -> Term
bEval t =
  fromMaybe t .
  bigStep $
  t

