{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Type inference for the B language.
-}
{-# LANGUAGE FlexibleContexts #-}
module Term.Infer (
    inferTermRules
  , inferTerm
  , runInfer
  ) where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError, throwError, Except, runExcept)

import Term (Term(..))
import Type (Type(..))
import Type.Error (TypeError(..), expect, expectEq)

-- |
inferTmFalse :: Monad m
             => Term           -- ^
             -> Maybe (m Type) -- ^
inferTmFalse TmFalse = Just $
  return TyBool
inferTmFalse _ =
  Nothing

-- |
inferTmTrue :: Monad m
            => Term           -- ^
            -> Maybe (m Type) -- ^
inferTmTrue TmTrue = Just $
  return TyBool
inferTmTrue _ =
  Nothing

-- |
inferTmIf :: MonadError TypeError m
          => (Term -> m Type)       -- ^
          -> Term                   -- ^
          -> Maybe (m Type)         -- ^
inferTmIf inferTerm (TmIf tm1 tm2 tm3) = Just $ do
  ty1 <- inferTerm tm1
  expect ty1 TyBool
  ty2 <- inferTerm tm2
  ty3 <- inferTerm tm3
  expectEq ty2 ty3
  return ty3
inferTmIf _ _ =
  Nothing

-- |
inferTermRules :: MonadError TypeError m
               => [Term -> Maybe (m Type)] -- ^
inferTermRules =
  [ inferTmFalse
  , inferTmTrue
  , inferTmIf inferTerm
  ]

-- |
inferTerm :: MonadError TypeError m
          => Term                    -- ^
          -> m Type                  -- ^
inferTerm tm =
  fromMaybe (throwError UnknownType) .
  asum .
  fmap ($ tm) $
  inferTermRules

-- |
runInfer :: Except e a -- ^
         -> Either e a -- ^
runInfer = runExcept
