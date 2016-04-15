{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Bool.Eval.BigStep (
    bigStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.BigStep (BigStepRule(..), BigStepInput(..))

import Components.Term.Bool (AsBoolTerm(..), WithBoolTerm)

-- |
eTrue :: WithBoolTerm tm a
      => tm a              -- ^
      -> Maybe (tm a)        -- ^
eTrue =
  fmap (review _TmTrue) .
  preview _TmTrue

-- |
eFalse :: WithBoolTerm tm a
       => tm a              -- ^
       -> Maybe (tm a)        -- ^
eFalse =
  fmap (review _TmFalse) .
  preview _TmFalse

-- |
eIfTrue :: WithBoolTerm tm a
        => (tm a -> Maybe (tm a)) -- ^
        -> tm a              -- ^
        -> Maybe (tm a)         -- ^
eIfTrue step tm = do
  (tm1, tm2, _) <- preview _TmIf tm
  tm1' <- step tm1
  _ <- preview _TmTrue tm1'
  step tm2

-- |
eIfFalse :: WithBoolTerm tm a
         => (tm a -> Maybe (tm a)) -- ^
         -> tm a              -- ^
         -> Maybe (tm a)         -- ^
eIfFalse step tm = do
  (tm1, _, tm3) <- preview _TmIf tm
  tm1' <- step tm1
  _ <- preview _TmFalse tm1'
  step tm3

-- |
bigStepInput :: WithBoolTerm tm a
             => BigStepInput (tm a)
bigStepInput =
  BigStepInput
    [ BigStepBase eFalse
    , BigStepBase eTrue
    , BigStepRecurse eIfFalse
    , BigStepRecurse eIfTrue
    ]
