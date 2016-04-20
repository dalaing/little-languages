{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Bool.Eval.BigStep (
    bigStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.BigStep (BigStepRule(..), BigStepInput(..))

import Component.Term.Bool (AsBoolTerm(..), WithBoolTerm)

-- |
eTrue :: WithBoolTerm tm n a
      => tm n a              -- ^
      -> Maybe (tm n a)        -- ^
eTrue =
  fmap (review _TmTrue) .
  preview _TmTrue

-- |
eFalse :: WithBoolTerm tm n a
       => tm n a              -- ^
       -> Maybe (tm n a)        -- ^
eFalse =
  fmap (review _TmFalse) .
  preview _TmFalse

-- |
eIfTrue :: WithBoolTerm tm n a
        => (tm n a -> Maybe (tm n a)) -- ^
        -> tm n a              -- ^
        -> Maybe (tm n a)         -- ^
eIfTrue step tm = do
  (tm1, tm2, _) <- preview _TmIf tm
  tm1' <- step tm1
  _ <- preview _TmTrue tm1'
  step tm2

-- |
eIfFalse :: WithBoolTerm tm n a
         => (tm n a -> Maybe (tm n a)) -- ^
         -> tm n a              -- ^
         -> Maybe (tm n a)         -- ^
eIfFalse step tm = do
  (tm1, _, tm3) <- preview _TmIf tm
  tm1' <- step tm1
  _ <- preview _TmFalse tm1'
  step tm3

-- |
bigStepInput :: WithBoolTerm tm n a
             => BigStepInput tm n a
bigStepInput =
  BigStepInput
    [ BigStepBase eFalse
    , BigStepBase eTrue
    , BigStepRecurse eIfFalse
    , BigStepRecurse eIfTrue
    ]
