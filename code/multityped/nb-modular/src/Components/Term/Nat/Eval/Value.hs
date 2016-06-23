module Components.Term.Nat.Eval.Value (
    nv
  , valueInput
  ) where

import Data.Foldable (asum)

import Control.Lens (preview, review)
import Control.Monad.Reader (ReaderT(..))

import Common.Recursion (Step(..))
import Common.Term.Eval.Value (ValueInput(..))

import Components.Term.Nat.Data

valueTmZero :: WithNatTerm ty tm
             => tm
             -> Maybe tm
valueTmZero =
  fmap (review _TmZero) .
  preview _TmZero

valueTmSucc :: WithNatTerm ty tm
             => tm
             -> Maybe tm
valueTmSucc tm = do
  n <- preview _TmSucc tm
  -- this is the strict version
  n' <- nv n
  return $ review _TmSucc n'

nv :: WithNatTerm ty tm
   => tm
   -> Maybe tm
nv tm =
  asum .
  map ($ tm) $
    [ valueTmZero
    , valueTmSucc
    ]

valueInput :: WithNatTerm ty tm
           => ValueInput tm
valueInput =
  ValueInput [SBase $ ReaderT nv]
