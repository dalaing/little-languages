{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Bool.Eval.SmallStep (
    smallStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.SmallStep (SmallStepRule(..), SmallStepInput(..))

import Component.Term.Bool (AsBoolTerm(..), WithBoolTerm)

-- |
eIfTrue :: WithBoolTerm tm n a
        => tm n a             -- ^
        -> Maybe (tm n a)        -- ^
eIfTrue tm = do
  (tm1, tm2, _) <- preview _TmIf tm
  _ <- preview _TmTrue tm1
  return tm2

-- |
eIfFalse :: WithBoolTerm tm n a
         => tm n a             -- ^
         -> Maybe (tm n a)        -- ^
eIfFalse tm = do
  (tm1, _, tm3) <- preview _TmIf tm
  _ <- preview _TmFalse tm1
  return tm3

-- |
eIf :: WithBoolTerm tm n a
    => (tm n a -> Maybe (tm n a)) -- ^
    -> tm n a              -- ^
    -> Maybe (tm n a)         -- ^
eIf step tm = do
  (tm1, tm2, tm3) <- preview _TmIf tm
  tm1' <- step tm1
  return $ review _TmIf (tm1', tm2, tm3)

-- |
smallStepInput :: WithBoolTerm tm n a
               => SmallStepInput tm n a
smallStepInput =
  SmallStepInput
    [ SmallStepBase eIfTrue
    , SmallStepBase eIfFalse
    , SmallStepRecurse eIf
    ]
