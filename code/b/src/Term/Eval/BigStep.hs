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

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Term (Term(..))

eTrue :: Term
      -> Maybe Term
eTrue TmTrue =
  Just TmTrue
eTrue _ =
  Nothing

eFalse :: Term
       -> Maybe Term
eFalse TmFalse =
  Just TmFalse
eFalse _ =
  Nothing

eIfTrue :: (Term -> Maybe Term)
        -> Term
        -> Maybe Term
eIfTrue step (TmIf t1 t2 _)
  | step t1 == Just TmTrue =
    step t2
  | otherwise =
    Nothing
eIfTrue _ _ =
  Nothing

eIfFalse :: (Term -> Maybe Term)
         -> Term
         -> Maybe Term
eIfFalse step (TmIf t1 _ t3)
  | step t1 == Just TmFalse =
    step t3
  | otherwise =
    Nothing
eIfFalse _ _ =
  Nothing

bigStepRules :: [Term -> Maybe Term]
bigStepRules =
  [ eTrue
  , eFalse
  , eIfTrue bigStep
  , eIfFalse bigStep
  ]

bigStep :: Term
        -> Maybe Term
bigStep tm =
  asum .
  fmap ($ tm) $
  bigStepRules

eval :: Term
     -> Term
eval tm =
  fromMaybe tm .
  bigStep $
  tm
