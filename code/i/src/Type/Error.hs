{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Type errors for the I language.
-}
{-# LANGUAGE FlexibleContexts      #-}
module Type.Error (
    TypeError (..)
  , expect
  , expectEq
  ) where

-- from 'base'
import           Control.Monad        (unless)

-- from 'mtl'
import           Control.Monad.Except (MonadError, throwError)

-- local
import           Type                 (Type)

-- | The type errors that can occur in the I language.
--
-- The 'NoMatchingTypeRule' constructor is acting like the 'Nothing' from 'Maybe'.
-- We could have left that constructor off and wrapped this type in 'Maybe', but
-- it's probably better to be explicit in this case. The hope is that doing so will
-- avoid a version of boolean-blindness.
data TypeError =
    Unexpected Type Type -- ^ A type was different to what was expected.
  | ExpectedEq Type Type -- ^ Two types that were expected to be equal but were not.
  | NoMatchingTypeRule          -- ^ An unknown type error.
  deriving (Eq, Ord, Show)

-- | A helper for asserting that a particular type is expected.

-- No examples because we only have one type at the moment :/
expect :: MonadError TypeError m
       => Type                   -- ^ The actual type
       -> Type                   -- ^ The expected type
       -> m ()
expect actual expected =
  unless (actual == expected) $
    throwError $ Unexpected actual expected

-- | A helper for asserting that two types are expected to be equal.

-- No examples because we only have one type at the moment :/
expectEq :: MonadError TypeError m
         => Type                   -- ^ The first type
         -> Type                   -- ^ The second type
         -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwError $ ExpectedEq ty1 ty2

