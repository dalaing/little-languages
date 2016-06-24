{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Type inference for the N language.
-}
{-# LANGUAGE FlexibleContexts      #-}
module Term.Infer (
    inferTermRules
  , inferTerm
  , runInfer
  ) where

-- from 'base'
import           Data.Foldable        (asum)
import           Data.Maybe           (fromMaybe)

-- from 'mtl'
import           Control.Monad.Except (Except, MonadError, runExcept,
                                       throwError)

-- local
import           Term                 (Term (..))
import           Type                 (Type (..))
import           Type.Error           (TypeError (..), expect)

-- | Infer the type of 'TmZero'.
--
-- >>> runInfer . fromMaybe (throwError NoMatchingTypeRule) . inferTmZero $ TmZero
-- Right TyNat
inferTmZero :: Monad m
            => Term
            -> Maybe (m Type)
inferTmZero TmZero = Just $
  return TyNat
inferTmZero _ =
  Nothing

-- | Infer the type of 'TmSucc'.
--
-- >>> runInfer . fromMaybe (throwError NoMatchingTypeRule) . (inferTmSucc inferTerm) $ TmSucc TmZero
-- Right TyNat

-- There will be better examples when we have more than one type
inferTmSucc :: MonadError TypeError m
            => (Term -> m Type) -- ^ The infer function for N.
            -> Term
            -> Maybe (m Type)
inferTmSucc infer (TmSucc tm) = Just $ do
  ty <- infer tm
  expect ty TyNat
  return TyNat
inferTmSucc _ _ =
  Nothing

-- | Infer the type of 'TmPred'.
--
-- >>> runInfer . fromMaybe (throwError NoMatchingTypeRule) . (inferTmPred inferTerm) $ TmPred TmZero
-- Right TyNat

-- There will be better examples when we have more than one type
inferTmPred :: MonadError TypeError m
            => (Term -> m Type) -- ^ The infer function for N.
            -> Term
            -> Maybe (m Type)
inferTmPred infer (TmPred tm) = Just $ do
  ty <- infer tm
  expect ty TyNat
  return TyNat
inferTmPred _ _ =
  Nothing

-- | The set of type inference rules for the N language.
inferTermRules :: MonadError TypeError m
               => [Term -> Maybe (m Type)]
inferTermRules =
  [ inferTmZero
  , inferTmSucc inferTerm
  , inferTmPred inferTerm
  ]

-- | The type inference function for the N language.
--
-- This function is built from the contents of 'inferTermRules'.
-- It will throw an 'NoMatchingTypeRule' error if none of the rules apply - which should never happen.
--
-- >>> runInfer . inferTerm $ TmZero
-- Right TyNat
--
-- >>> runInfer . inferTerm $ TmSucc (TmSucc (TmPred TmZero))
-- Right TyNat
inferTerm :: MonadError TypeError m
          => Term
          -> m Type
inferTerm tm =
  fromMaybe (throwError NoMatchingTypeRule) .
  asum .
  fmap ($ tm) $
  inferTermRules

-- | This function interprets values in the type inference monad.
--
-- Note that using this selects a particular instance of 'MonadError', which
-- may not always be what you want.
runInfer :: Except e a
         -> Either e a
runInfer = runExcept
