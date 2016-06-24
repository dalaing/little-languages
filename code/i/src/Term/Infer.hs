{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Type inference for the I language.
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

-- | Infer the type of 'TmInt'.
--
-- >>> runInfer . fromMaybe (throwError NoMatchingTypeRule) . inferTmInt $ (TmInt 3)
-- Right TyInt
inferTmInt :: Monad m
             => Term
             -> Maybe (m Type)
inferTmInt (TmInt _) = Just $
  return TyInt
inferTmInt _ =
  Nothing

-- | Infer the type of 'TmAdd'.
--
-- >>> runInfer . fromMaybe (throwError NoMatchingTypeRule) . (inferTmAdd inferTerm) $ (TmAdd (TmInt 2) (TmInt 5))
-- Right TyInt
inferTmAdd :: MonadError TypeError m
           => (Term -> m Type)       -- ^ The infer function for I.
           -> Term
           -> Maybe (m Type)
inferTmAdd infer (TmAdd tm1 tm2) = Just $ do
  ty1 <- infer tm1
  expect ty1 TyInt
  ty2 <- infer tm2
  expect ty2 TyInt
  return TyInt
inferTmAdd _ _ =
  Nothing

-- | Infer the type of 'TmSub'.
--
-- >>> runInfer . fromMaybe (throwError NoMatchingTypeRule) . (inferTmSub inferTerm) $ (TmSub (TmInt 2) (TmInt 5))
-- Right TyInt
inferTmSub :: MonadError TypeError m
           => (Term -> m Type)       -- ^ The infer function for I.
           -> Term
           -> Maybe (m Type)
inferTmSub infer (TmSub tm1 tm2) = Just $ do
  ty1 <- infer tm1
  expect ty1 TyInt
  ty2 <- infer tm2
  expect ty2 TyInt
  return TyInt
inferTmSub _ _ =
  Nothing

-- | Infer the type of 'TmMul'.
--
-- >>> runInfer . fromMaybe (throwError NoMatchingTypeRule) . (inferTmMul inferTerm) $ (TmMul (TmInt 2) (TmInt 5))
-- Right TyInt
inferTmMul :: MonadError TypeError m
           => (Term -> m Type)       -- ^ The infer function for I.
           -> Term
           -> Maybe (m Type)
inferTmMul infer (TmMul tm1 tm2) = Just $ do
  ty1 <- infer tm1
  expect ty1 TyInt
  ty2 <- infer tm2
  expect ty2 TyInt
  return TyInt
inferTmMul _ _ =
  Nothing

-- | Infer the type of 'TmExp'.
--
-- >>> runInfer . fromMaybe (throwError NoMatchingTypeRule) . (inferTmExp inferTerm) $ (TmExp (TmInt 2) (TmInt 5))
-- Right TyInt
inferTmExp :: MonadError TypeError m
           => (Term -> m Type)       -- ^ The infer function for I.
           -> Term
           -> Maybe (m Type)
inferTmExp infer (TmExp tm1 tm2) = Just $ do
  ty1 <- infer tm1
  expect ty1 TyInt
  ty2 <- infer tm2
  expect ty2 TyInt
  return TyInt
inferTmExp _ _ =
  Nothing

-- | The set of type inference rules for the I language.
inferTermRules :: MonadError TypeError m
               => [Term -> Maybe (m Type)]
inferTermRules =
  [ inferTmInt
  , inferTmAdd inferTerm
  , inferTmSub inferTerm
  , inferTmMul inferTerm
  , inferTmExp inferTerm
  ]

-- | The type inference function for the I language.
--
-- This function is built from the contents of 'inferTermRules'.
-- It will throw an 'NoMatchingTypeRule' error if none of the rules apply - which should never happen.
--
-- >>> runInfer . inferTerm $ TmInt 3
-- Right TyInt
--
-- >>> runInfer . inferTerm $ TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5))
-- Right TyInt
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
