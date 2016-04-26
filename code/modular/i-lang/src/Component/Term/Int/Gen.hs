{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Int.Gen (
    genTermInput
  ) where

import           Control.Lens         (preview, review)
import           Test.QuickCheck      (Gen, Arbitrary(..))

import Component.Term.Gen (GenAnyTermRule(..), ShrAnyTermRule(..), GenTermInput(..), GenContainingTermRule(..), GenWellTypedTermRule(..), GenIllTypedTermRule(..))

import           Component.Term.Int (AsIntTerm (..), WithIntTerm)
import           Component.Type.Int (AsIntType (..), WithIntType)

-- |
genTmInt :: WithIntTerm tm n a
         => Gen (tm n a)
genTmInt =
   review _TmIntLit <$> arbitrary

-- |
shrinkTmInt :: WithIntTerm tm n a
            => tm n a        -- ^
            -> Maybe [tm n a] -- ^
shrinkTmInt =
    fmap shrinkTmInt' .
    preview _TmIntLit
  where
    shrinkTmInt' =
      fmap (review _TmIntLit) .
      shrink

-- |
genTmAdd :: WithIntTerm tm n a
          => Gen (tm n a)
          -> Gen (tm n a)
          -> Gen (tm n a)
genTmAdd g1 g2 =
  curry (review _TmAdd) <$> g1 <*> g2

-- |
shrinkTmAdd :: WithIntTerm tm n a
             => (tm n a -> [tm n a])
             -> tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmAdd s =
    fmap shrinkTmAdd' .
    preview _TmAdd
  where
    shrinkTmAdd' (tm1, tm2) =
      s tm1 ++ [tm1] ++
      s tm2 ++ [tm2] ++
      fmap (\tm1' -> review _TmAdd (tm1', tm2)) (s tm1) ++
      fmap (\tm2' -> review _TmAdd (tm1, tm2')) (s tm2)

-- |
genTmSub :: WithIntTerm tm n a
          => Gen (tm n a)
          -> Gen (tm n a)
          -> Gen (tm n a)
genTmSub g1 g2 =
  curry (review _TmSub) <$> g1 <*> g2

-- |
shrinkTmSub :: WithIntTerm tm n a
             => (tm n a -> [tm n a])
             -> tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmSub s =
    fmap shrinkTmSub' .
    preview _TmSub
  where
    shrinkTmSub' (tm1, tm2) =
      s tm1 ++ [tm1] ++
      s tm2 ++ [tm2] ++
      fmap (\tm1' -> review _TmSub (tm1', tm2)) (s tm1) ++
      fmap (\tm2' -> review _TmSub (tm1, tm2')) (s tm2)

-- |
genTmMul :: WithIntTerm tm n a
          => Gen (tm n a)
          -> Gen (tm n a)
          -> Gen (tm n a)
genTmMul g1 g2 =
  curry (review _TmMul) <$> g1 <*> g2

-- |
shrinkTmMul :: WithIntTerm tm n a
             => (tm n a -> [tm n a])
             -> tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmMul s =
    fmap shrinkTmMul' .
    preview _TmMul
  where
    shrinkTmMul' (tm1, tm2) =
      s tm1 ++ [tm1] ++
      s tm2 ++ [tm2] ++
      fmap (\tm1' -> review _TmMul (tm1', tm2)) (s tm1) ++
      fmap (\tm2' -> review _TmMul (tm1, tm2')) (s tm2)

-- |
genTmExp :: WithIntTerm tm n a
          => Gen (tm n a)
          -> Gen (tm n a)
          -> Gen (tm n a)
genTmExp g1 g2 =
  curry (review _TmExp) <$> g1 <*> g2

-- |
shrinkTmExp :: WithIntTerm tm n a
             => (tm n a -> [tm n a])
             -> tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmExp s =
    fmap shrinkTmExp' .
    preview _TmExp
  where
    shrinkTmExp' (tm1, tm2) =
      s tm1 ++ [tm1] ++
      s tm2 ++ [tm2] ++
      fmap (\tm1' -> review _TmExp (tm1', tm2)) (s tm1) ++
      fmap (\tm2' -> review _TmExp (tm1, tm2')) (s tm2)

genContainingTmInt :: ( WithIntType ty nTy
                      , WithIntTerm tm nTm a
                      )
                   => tm nTm a
                   -> ty nTy
                   -> Maybe (Gen (tm nTm a))
genContainingTmInt tm ty = do
  i <- preview _TmIntLit tm
  _ <- preview _TyInt ty
  return . pure $ review _TmIntLit i

genContainingTmAdd1 :: ( WithIntType ty nTy
                       , WithIntTerm tm nTm a
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmAdd1 genWellTyped genContaining tm ty s =
    fmap genContainingTmAdd1' .
    preview _TyInt $
    ty
  where
    genContainingTmAdd1' _ = do
      let s' = s `div` 2
      tm1 <- genContaining tm (review _TyInt ()) s'
      tm2 <- genWellTyped (review _TyInt ()) s'
      return $ review _TmAdd (tm1, tm2)

genContainingTmAdd2 :: ( WithIntType ty nTy
                       , WithIntTerm tm nTm a
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmAdd2 genWellTyped genContaining tm ty s =
    fmap genContainingTmAdd2' .
    preview _TyInt $
    ty
  where
    genContainingTmAdd2' _ = do
      let s' = s `div` 2
      tm1 <- genWellTyped (review _TyInt ()) s'
      tm2 <- genContaining tm (review _TyInt ()) s'
      return $ review _TmAdd (tm1, tm2)

genContainingTmSub1 :: ( WithIntType ty nTy
                       , WithIntTerm tm nTm a
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmSub1 genWellTyped genContaining tm ty s =
    fmap genContainingTmSub1' .
    preview _TyInt $
    ty
  where
    genContainingTmSub1' _ = do
      let s' = s `div` 2
      tm1 <- genContaining tm (review _TyInt ()) s'
      tm2 <- genWellTyped (review _TyInt ()) s'
      return $ review _TmSub (tm1, tm2)

genContainingTmSub2 :: ( WithIntType ty nTy
                       , WithIntTerm tm nTm a
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmSub2 genWellTyped genContaining tm ty s =
    fmap genContainingTmSub2' .
    preview _TyInt $
    ty
  where
    genContainingTmSub2' _ = do
      let s' = s `div` 2
      tm1 <- genWellTyped (review _TyInt ()) s'
      tm2 <- genContaining tm (review _TyInt ()) s'
      return $ review _TmSub (tm1, tm2)

genContainingTmMul1 :: ( WithIntType ty nTy
                       , WithIntTerm tm nTm a
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmMul1 genWellTyped genContaining tm ty s =
    fmap genContainingTmMul1' .
    preview _TyInt $
    ty
  where
    genContainingTmMul1' _ = do
      let s' = s `div` 2
      tm1 <- genContaining tm (review _TyInt ()) s'
      tm2 <- genWellTyped (review _TyInt ()) s'
      return $ review _TmMul (tm1, tm2)

genContainingTmMul2 :: ( WithIntType ty nTy
                       , WithIntTerm tm nTm a
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmMul2 genWellTyped genContaining tm ty s =
    fmap genContainingTmMul2' .
    preview _TyInt $
    ty
  where
    genContainingTmMul2' _ = do
      let s' = s `div` 2
      tm1 <- genWellTyped (review _TyInt ()) s'
      tm2 <- genContaining tm (review _TyInt ()) s'
      return $ review _TmMul (tm1, tm2)

genContainingTmExp1 :: ( WithIntType ty nTy
                       , WithIntTerm tm nTm a
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmExp1 genWellTyped genContaining tm ty s =
    fmap genContainingTmExp1' .
    preview _TyInt $
    ty
  where
    genContainingTmExp1' _ = do
      let s' = s `div` 2
      tm1 <- genContaining tm (review _TyInt ()) s'
      tm2 <- genWellTyped (review _TyInt ()) s'
      return $ review _TmExp (tm1, tm2)

genContainingTmExp2 :: ( WithIntType ty nTy
                       , WithIntTerm tm nTm a
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmExp2 genWellTyped genContaining tm ty s =
    fmap genContainingTmExp2' .
    preview _TyInt $
    ty
  where
    genContainingTmExp2' _ = do
      let s' = s `div` 2
      tm1 <- genWellTyped (review _TyInt ()) s'
      tm2 <- genContaining tm (review _TyInt ()) s'
      return $ review _TmExp (tm1, tm2)

genWellTypedTmInt :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => ty nTy
                  -> Maybe (Gen (tm nTm a))
genWellTypedTmInt ty = do
  _ <- preview _TyInt ty
  return (review _TmIntLit <$> arbitrary)

genWellTypedTmAdd :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genWellTypedTmAdd genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyInt ()) s'
    tm2 <- genWellTyped (review _TyInt ()) s'
    return $ review _TmAdd (tm1, tm2)

genWellTypedTmSub :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genWellTypedTmSub genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyInt ()) s'
    tm2 <- genWellTyped (review _TyInt ()) s'
    return $ review _TmSub (tm1, tm2)

genWellTypedTmMul :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genWellTypedTmMul genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyInt ()) s'
    tm2 <- genWellTyped (review _TyInt ()) s'
    return $ review _TmMul (tm1, tm2)

genWellTypedTmExp :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genWellTypedTmExp genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyInt ()) s'
    tm2 <- genWellTyped (review _TyInt ()) s'
    return $ review _TmExp (tm1, tm2)

genIllTypedTmAdd1 :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmAdd1 genNotType genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    nty <- genNotType (review _TyInt ())
    tm1 <- genWellTyped nty s'
    tm2 <- genWellTyped (review _TyInt ()) s'
    return $ review _TmAdd (tm1, tm2)

genIllTypedTmAdd2 :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmAdd2 genNotType genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyInt ()) s'
    nty <- genNotType (review _TyInt ())
    tm2 <- genWellTyped nty s'
    return $ review _TmAdd (tm1, tm2)

genIllTypedTmSub1 :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmSub1 genNotType genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    nty <- genNotType (review _TyInt ())
    tm1 <- genWellTyped nty s'
    tm2 <- genWellTyped (review _TyInt ()) s'
    return $ review _TmSub (tm1, tm2)

genIllTypedTmSub2 :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmSub2 genNotType genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyInt ()) s'
    nty <- genNotType (review _TyInt ())
    tm2 <- genWellTyped nty s'
    return $ review _TmSub (tm1, tm2)

genIllTypedTmMul1 :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmMul1 genNotType genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    nty <- genNotType (review _TyInt ())
    tm1 <- genWellTyped nty s'
    tm2 <- genWellTyped (review _TyInt ()) s'
    return $ review _TmMul (tm1, tm2)

genIllTypedTmMul2 :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmMul2 genNotType genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyInt ()) s'
    nty <- genNotType (review _TyInt ())
    tm2 <- genWellTyped nty s'
    return $ review _TmMul (tm1, tm2)

genIllTypedTmExp1 :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmExp1 genNotType genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    nty <- genNotType (review _TyInt ())
    tm1 <- genWellTyped nty s'
    tm2 <- genWellTyped (review _TyInt ()) s'
    return $ review _TmExp (tm1, tm2)

genIllTypedTmExp2 :: ( WithIntType ty nTy
                     , WithIntTerm tm nTm a
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmExp2 genNotType genWellTyped ty s = do
  _ <- preview _TyInt ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyInt ()) s'
    nty <- genNotType (review _TyInt ())
    tm2 <- genWellTyped nty s'
    return $ review _TmExp (tm1, tm2)

genTermInput :: ( WithIntType ty nTy
                , WithIntTerm tm nTm a
                )
             => GenTermInput ty nTy tm nTm a
genTermInput =
   GenTermInput
    [ GenAnyTermBase genTmInt
    , gatr genTmAdd
    , gatr genTmSub
    , gatr genTmMul
    , gatr genTmExp
    ]
    [ ShrAnyTermBase shrinkTmInt
    , ShrAnyTermRecurse shrinkTmAdd
    , ShrAnyTermRecurse shrinkTmSub
    , ShrAnyTermRecurse shrinkTmMul
    , ShrAnyTermRecurse shrinkTmExp
    ]
    [ GenContainingTermBase genContainingTmInt
    , GenContainingTermRecurse genContainingTmAdd1
    , GenContainingTermRecurse genContainingTmAdd2
    , GenContainingTermRecurse genContainingTmSub1
    , GenContainingTermRecurse genContainingTmSub2
    , GenContainingTermRecurse genContainingTmMul1
    , GenContainingTermRecurse genContainingTmMul2
    , GenContainingTermRecurse genContainingTmExp1
    , GenContainingTermRecurse genContainingTmExp2
    ]
    [ GenWellTypedTermBase genWellTypedTmInt
    , GenWellTypedTermRecurse genWellTypedTmAdd
    , GenWellTypedTermRecurse genWellTypedTmSub
    , GenWellTypedTermRecurse genWellTypedTmMul
    , GenWellTypedTermRecurse genWellTypedTmExp
    ]
    [ GenIllTypedTermRecurse genIllTypedTmAdd1
    , GenIllTypedTermRecurse genIllTypedTmAdd2
    , GenIllTypedTermRecurse genIllTypedTmSub1
    , GenIllTypedTermRecurse genIllTypedTmSub2
    , GenIllTypedTermRecurse genIllTypedTmMul1
    , GenIllTypedTermRecurse genIllTypedTmMul2
    , GenIllTypedTermRecurse genIllTypedTmExp1
    , GenIllTypedTermRecurse genIllTypedTmExp2
    ]
  where
    gatr g2 = GenAnyTermRecurse $ \g s ->
      let
        s' = s `div` 2
      in
        g2 (g s') (g s')
