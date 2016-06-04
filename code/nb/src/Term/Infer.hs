{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Type inference for the NB language.
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
import           Type.Error           (TypeError (..), expect, expectEq)

-- | Infer the type of 'TmZero'.
--
-- >>> runInfer . fromMaybe (throwError UnknownType) . inferTmZero $ TmZero
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
-- >>> runInfer . fromMaybe (throwError UnknownType) . (inferTmSucc inferTerm) $ TmSucc TmZero
-- Right TyNat

-- There will be better examples when we have more than one type
inferTmSucc :: MonadError TypeError m
            => (Term -> m Type) -- ^ The infer function for NB.
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
-- >>> runInfer . fromMaybe (throwError UnknownType) . (inferTmPred inferTerm) $ TmPred TmZero
-- Right TyNat

-- There will be better examples when we have more than one type
inferTmPred :: MonadError TypeError m
            => (Term -> m Type) -- ^ The infer function for NB.
            -> Term
            -> Maybe (m Type)
inferTmPred infer (TmPred tm) = Just $ do
  ty <- infer tm
  expect ty TyNat
  return TyNat
inferTmPred _ _ =
  Nothing

-- | Infer the type of 'TmFalse'.
--
-- >>> runInfer . fromMaybe (throwError UnknownType) . inferTmFalse $ TmFalse
-- Right TyBool
inferTmFalse :: Monad m
             => Term
             -> Maybe (m Type)
inferTmFalse TmFalse = Just $
  return TyBool
inferTmFalse _ =
  Nothing

-- | Infer the type of 'TmFalse'.
--
-- >>> runInfer . fromMaybe (throwError UnknownType) . inferTmTrue $ TmTrue
-- Right TyBool
inferTmTrue :: Monad m
            => Term
            -> Maybe (m Type)
inferTmTrue TmTrue = Just $
  return TyBool
inferTmTrue _ =
  Nothing

-- | Infer the type of 'TmIf'.
--
-- >>> runInfer . fromMaybe (throwError UnknownType) . (inferTmIf inferTerm) $ TmIf TmFalse TmFalse TmTrue
-- Right TyBool

-- There will be better examples when we have more than one type
inferTmIf :: MonadError TypeError m
          => (Term -> m Type)       -- ^ The infer function for NB.
          -> Term
          -> Maybe (m Type)
inferTmIf infer (TmIf tm1 tm2 tm3) = Just $ do
  ty1 <- infer tm1
  expect ty1 TyBool
  ty2 <- infer tm2
  ty3 <- infer tm3
  expectEq ty2 ty3
  return ty3
inferTmIf _ _ =
  Nothing

-- | Infer the type of 'TmIsZero'.
--
-- >>> runInfer . fromMaybe (throwError UnknownType) . (inferTmIsZero inferTerm) $ TmIsZero (TmSucc TmZero)
-- Right TyBool

-- There will be better examples when we have more than one type
inferTmIsZero :: MonadError TypeError m
              => (Term -> m Type) -- ^ The infer function for NB.
              -> Term
              -> Maybe (m Type)
inferTmIsZero infer (TmIsZero tm) = Just $ do
  ty <- infer tm
  expect ty TyNat
  return TyBool
inferTmIsZero _ _ =
  Nothing

-- | The set of type inference rules for the NB language.
inferTermRules :: MonadError TypeError m
               => [Term -> Maybe (m Type)]
inferTermRules =
  [ inferTmZero
  , inferTmSucc inferTerm
  , inferTmPred inferTerm
  , inferTmFalse
  , inferTmTrue
  , inferTmIf inferTerm
  , inferTmIsZero inferTerm
  ]

-- | The type inference function for the NB language.
--
-- This function is built from the contents of 'inferTermRules'.
-- It will throw an 'UnknownType' error if none of the rules apply - which should never happen.
--
-- >>> runInfer . inferTerm $ TmZero
-- Right TyNat
--
-- >>> runInfer . inferTerm $ TmSucc (TmSucc (TmPred TmZero))
-- Right TyNat
--
-- >>> runInfer . inferTerm $ TmTrue
-- Right TyBool
--
-- >>> runInfer . inferTerm $ TmIf TmFalse TmFalse TmTrue
-- Right TyBool
--
-- >>> runInfer . inferTerm $ TmIsZero (TmSucc TmZero)
-- Right TyBool
inferTerm :: MonadError TypeError m
          => Term
          -> m Type
inferTerm tm =
  fromMaybe (throwError UnknownType) .
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
