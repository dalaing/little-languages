module Type.Gen where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import Control.Lens (preview, review)
import Test.QuickCheck (Gen, sized, oneof)

import Type

genTyBool :: Gen (Type l)
genTyBool = pure (review _TyBool ())

genTyArr :: Gen (Type l)
         -> Gen (Type l)
         -> Gen (Type l)
genTyArr g1 g2 = curry (review _TyArr) <$> g1 <*> g2

genType :: Gen (Type l)
genType = sized genType'

genType' :: Int -> Gen (Type l)
genType' s =
  let
    zeroSize = [genTyBool]
    child = genType' (s `div` 2)
    nonZeroSize = [genTyArr child child]
  in
    oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

shrinkTyBool :: Type l
             -> Maybe [Type l]
shrinkTyBool = fmap (const []) . preview _TyBool

shrinkTyArr :: (Type l -> [Type l])
            -> (Type l -> [Type l])
            -> Type l
            -> Maybe ([Type l])
shrinkTyArr shr1 shr2 = fmap shrinkTyArr' . preview _TyArr
  where
    shrinkTyArr' (t1, t2) =
      shr1 t1 ++
      shr2 t2 ++
      fmap (\u1 -> review _TyArr (u1, t2)) (shr1 t1) ++
      fmap (\u2 -> review _TyArr (t1, u2)) (shr2 t2)

shrinkType :: Type l -> [Type l]
shrinkType t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    shrinkTyBool
  , shrinkTyArr shrinkType shrinkType
  ]
