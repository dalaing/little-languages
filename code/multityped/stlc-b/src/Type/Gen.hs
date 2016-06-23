module Type.Gen where


import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Lens (preview)

import Text.Trifecta.Rendering (Span)

import Test.QuickCheck (Gen, Arbitrary(..), sized, oneof)

import Loc
import Type

genTyBool :: Gen (Type l)
genTyBool = pure TyBool

genTyArr :: Gen (Type l)
         -> Gen (Type l)
         -> Gen (Type l)
genTyArr g1 g2 =
  TyArr <$> g1 <*> g2

genType :: Gen (Type l)
genType =
  sized genType'

genType' :: Int
         -> Gen (Type l)
genType' s =
  let
    zeroSize = [genTyBool]
    s' = s `div` 2
    child = genType' s'
    nonZeroSize = [genTyArr child child]
  in
    oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

genNotTyBool :: Type l
             -> [Gen (Type l)]
genNotTyBool TyBool =
  []
genNotTyBool _      =
  [genTyBool]

genNotTyArr :: Gen (Type l)
            -> (Type l -> Gen (Type l))
            -> Type l
            -> [Gen (Type l)]
genNotTyArr gAny gNot (TyArr t1 t2) =
  [ genTyArr (gNot t1) (pure t2)
  , genTyArr (pure t1) (gNot t2)
  , genTyArr (gNot t1) (gNot t2)
  ]
genNotTyArr gAny gNot _ =
  [ genTyArr gAny gAny ]

genNotType :: Type l
           -> Gen (Type l)
genNotType ty =
  sized $ \s ->
    genNotType' s ty

-- TODO do we need something for tyloc?
genNotType' :: Int
            -> Type l
            -> Gen (Type l)
genNotType' s ty =
  let
    zeroSize = [genNotTyBool]
    s' = s `div` 2
    childNot = genNotType' s'
    childAny = genType' s'
    nonZeroSize = [genNotTyArr childAny childNot]
  in
    oneof . concatMap ($ ty) $ (if s == 0 then [] else nonZeroSize) ++ zeroSize


shrinkTyBool :: Type l
             -> Maybe [Type l]
shrinkTyBool =
  fmap (const []) .
  preview _TyBool

shrinkTyArr :: (Type l -> [Type l])
            -> (Type l -> [Type l])
            -> Type l
            -> Maybe [Type l]
shrinkTyArr shr1 shr2 =
    fmap shrinkTyArr' .
    preview _TyArr
  where
    shrinkTyArr' (t1, t2) =
      fmap (\u1 -> TyArr u1 t2) (shr1 t1) ++
      fmap (\u2 -> TyArr t1 u2) (shr2 t2)

shrinkType :: Type l -> [Type l]
shrinkType t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    shrinkTyBool
  , shrinkTyArr shrinkType shrinkType
  ]

newtype AnyType = AnyType { getAnyType :: Type Span }
                deriving (Show)

instance Eq AnyType where
  AnyType x == AnyType y =
    stripLoc x == stripLoc y

instance Ord AnyType where
  compare (AnyType x) (AnyType y) =
    compare (stripLoc x) (stripLoc y)

instance Arbitrary AnyType where
  arbitrary = AnyType <$> genType
  shrink = fmap AnyType . shrinkType . getAnyType
