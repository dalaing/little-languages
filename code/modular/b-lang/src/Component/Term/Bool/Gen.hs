{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Bool.Gen (
    genTermInput
  ) where

import           Control.Lens         (preview, review)
import           Test.QuickCheck      (Gen)

import Component.Term.Gen (GenAnyTermRule(..), ShrAnyTermRule(..), GenContainingTermRule(..), GenWellTypedTermRule(..), GenIllTypedTermRule(..), GenTermInput(..))

import           Component.Type.Bool (AsBoolType (..), WithBoolType)
import           Component.Term.Bool (AsBoolTerm (..), WithBoolTerm)

-- |
genTmFalse :: WithBoolTerm tm
           => Gen (tm n a)
genTmFalse =
  pure $ review _TmFalse ()

-- |
shrinkTmFalse :: WithBoolTerm tm
              => tm n a        -- ^
              -> Maybe [tm n a] -- ^
shrinkTmFalse =
  fmap (const []) .
  preview _TmFalse

-- |
genTmTrue :: WithBoolTerm tm
          => Gen (tm n a)
genTmTrue =
  pure $ review _TmTrue ()

-- |
shrinkTmTrue :: WithBoolTerm tm
             => tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmTrue =
  fmap (const []) .
  preview _TmTrue

-- |
genTmIf :: WithBoolTerm tm
        => Gen (tm n a) -- ^
        -> Gen (tm n a) -- ^
        -> Gen (tm n a) -- ^
        -> Gen (tm n a) -- ^
genTmIf g1 g2 g3 =
  fmap (review _TmIf)
    ((,,) <$> g1 <*> g2 <*> g3)

-- |
shrinkTmIf :: WithBoolTerm tm
           => (tm n a -> [tm n a]) -- ^
           -> (tm n a -> [tm n a]) -- ^
           -> (tm n a -> [tm n a]) -- ^
           -> tm n a          -- ^
           -> Maybe [tm n a]   -- ^
shrinkTmIf s1 s2 s3 =
    fmap shrinkTmIf' .
    preview _TmIf
  where
    shrinkTmIf' (tm1, tm2, tm3) =
      s1 tm1 ++ [tm1] ++
      s2 tm2 ++ [tm2] ++
      s3 tm3 ++ [tm3] ++
      fmap (\tm1' -> review _TmIf (tm1', tm2, tm3)) (s1 tm1) ++
      fmap (\tm2' -> review _TmIf (tm1, tm2', tm3)) (s2 tm2) ++
      fmap (\tm3' -> review _TmIf (tm1, tm2, tm3')) (s3 tm3)

genContainingIf :: ( WithBoolType ty
                   , WithBoolTerm tm
                   )
                => (ty nTy -> Int -> Gen (tm nTm a))
                -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                -> tm nTm a
                -> ty nTy
                -> Int
                -> Maybe (Gen (tm nTm a))
genContainingIf genWellTyped genContaining tm ty s = Just $ do
  let s' = s `div` 3
  tm1 <- genContaining tm (review _TyBool ()) s'
  tm2 <- genWellTyped ty s'
  tm3 <- genWellTyped ty s'
  return $ review _TmIf (tm1, tm2, tm3)

genContainingThen :: ( WithBoolType ty
                     , WithBoolTerm tm
                     )
                  => (ty nTy -> Int -> Gen (tm nTm a))
                  -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                  -> tm nTm a
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genContainingThen genWellTyped genContaining tm ty s = Just $ do
  let s' = s `div` 3
  tm1 <- genWellTyped (review _TyBool ()) s'
  tm2 <- genContaining tm ty s'
  tm3 <- genWellTyped ty s'
  return $ review _TmIf (tm1, tm2, tm3)

genContainingElse :: ( WithBoolType ty
                     , WithBoolTerm tm
                     )
                  => (ty nTy -> Int -> Gen (tm nTm a))
                  -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                  -> tm nTm a
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genContainingElse genWellTyped genContaining tm ty s = Just $ do
  let s' = s `div` 3
  tm1 <- genWellTyped (review _TyBool ()) s'
  tm2 <- genWellTyped ty s'
  tm3 <- genContaining tm ty s'
  return $ review _TmIf (tm1, tm2, tm3)

genWellTypedTmFalse :: ( WithBoolType ty
                       , WithBoolTerm tm
                       )
                    => ty nTy
                    -> Maybe (Gen (tm nTm a))
genWellTypedTmFalse ty = do
  _ <- preview _TyBool ty
  return . pure $ review _TmFalse ()

genWellTypedTmTrue :: ( WithBoolType ty
                      , WithBoolTerm tm
                      )
                   => ty nTy
                   -> Maybe (Gen (tm nTm a))
genWellTypedTmTrue ty = do
  _ <- preview _TyBool ty
  return . pure $ review _TmTrue ()

genWellTypedTmIf :: ( WithBoolType ty
                    , WithBoolTerm tm
                    )
                 => (ty nTy -> Int -> Gen (tm nTm a))
                 -> ty nTy
                 -> Int
                 -> Maybe (Gen (tm nTm a))
genWellTypedTmIf genWellTyped ty s = Just $ do
  let s' = s `div` 2
  tm1 <- genWellTyped (review _TyBool ()) s'
  tm2 <- genWellTyped ty s'
  tm3 <- genWellTyped ty s'
  return $ review _TmIf (tm1, tm2, tm3)

genIllTypedIf :: ( WithBoolType ty
                 , WithBoolTerm tm
                 )
              => (ty nTy -> Gen (ty nTy))
              -> (ty nTy -> Int -> Gen (tm nTm a))
              -> ty nTy
              -> Int
              -> Maybe (Gen (tm nTm a))
genIllTypedIf genNotType genWellTyped ty s = Just $ do
  let s' = s `div` 2
  nty <- genNotType (review _TyBool ())
  tm1 <- genWellTyped nty s'
  tm2 <- genWellTyped ty s'
  tm3 <- genWellTyped ty s'
  return $ review _TmIf (tm1, tm2, tm3)

genIllTypedThen :: ( WithBoolType ty
                   , WithBoolTerm tm
                   )
                => (ty nTy -> Gen (ty nTy))
                -> (ty nTy -> Int -> Gen (tm nTm a))
                -> ty nTy
                -> Int
                -> Maybe (Gen (tm nTm a))
genIllTypedThen genNotType genWellTyped ty s = Just $ do
  let s' = s `div` 2
  tm1 <- genWellTyped (review _TyBool ()) s'
  nty <- genNotType ty
  tm2 <- genWellTyped nty s'
  tm3 <- genWellTyped ty s'
  return $ review _TmIf (tm1, tm2, tm3)

genIllTypedElse :: ( WithBoolType ty
                   , WithBoolTerm tm
                   )
                => (ty nTy -> Gen (ty nTy))
                -> (ty nTy -> Int -> Gen (tm nTm a))
                -> ty nTy
                -> Int
                -> Maybe (Gen (tm nTm a))
genIllTypedElse genNotType genWellTyped ty s = Just $ do
  let s' = s `div` 2
  tm1 <- genWellTyped (review _TyBool ()) s'
  tm2 <- genWellTyped ty s'
  nty <- genNotType ty
  tm3 <- genWellTyped nty s'
  return $ review _TmIf (tm1, tm2, tm3)

genTermInput :: ( WithBoolTerm tm 
                , WithBoolType ty
                )
             => GenTermInput ty nTy tm nTm a
genTermInput =
  GenTermInput
    [ GenAnyTermBase genTmFalse
    , GenAnyTermBase genTmTrue
    , GenAnyTermRecurse $ \g s ->
        let
          child = g (s `div` 3)
        in
          genTmIf child child child
    ]
    [ ShrAnyTermBase shrinkTmFalse
    , ShrAnyTermBase shrinkTmTrue
    , ShrAnyTermRecurse $ \s -> shrinkTmIf s s s
    ]
    [ GenContainingTermRecurse genContainingIf
    , GenContainingTermRecurse genContainingThen
    , GenContainingTermRecurse genContainingElse
    ]
    [ GenWellTypedTermBase genWellTypedTmFalse
    , GenWellTypedTermBase genWellTypedTmTrue
    , GenWellTypedTermRecurse genWellTypedTmIf
    ]
    [ GenIllTypedTermRecurse genIllTypedIf
    , GenIllTypedTermRecurse genIllTypedThen
    , GenIllTypedTermRecurse genIllTypedElse
    ]
