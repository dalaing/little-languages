module Term.Gen where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

import Term

genTmInt :: Gen Term
genTmInt = TmInt <$> arbitrary

genTmAdd :: Gen Term -> Gen Term -> Gen Term
genTmAdd g1 g2 = TmAdd <$> g1 <*> g2

genTmSub :: Gen Term -> Gen Term -> Gen Term
genTmSub g1 g2 = TmSub <$> g1 <*> g2

genTmMul :: Gen Term -> Gen Term -> Gen Term
genTmMul g1 g2 = TmMul <$> g1 <*> g2

genTmExp :: Gen Term -> Gen Term -> Gen Term
genTmExp g1 g2 = TmExp <$> g1 <*> g2

genTerm :: Gen Term
genTerm = sized genTerm'
  where
    genTerm' s =
      let
        zeroSize = [genTmInt]
        child = genTerm' (s `div` 2)
        nonZeroSize = [genTmAdd child child, genTmSub child child, genTmMul child child, genTmExp child child]
      in
        oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

shrTmInt :: Term -> Maybe [Term]
shrTmInt = fmap (fmap TmInt . shrink) . preview _TmInt

shrTmAdd :: (Term -> [Term]) -> (Term -> [Term]) -> Term -> Maybe [Term]
shrTmAdd shr1 shr2 = fmap shrTmAdd' . preview _TmAdd
  where
    shrTmAdd' (x, y) =
      shr1 x ++
      shr2 y ++
      fmap (\x' -> TmAdd x' y) (shr1 x) ++
      fmap (\y' -> TmAdd x y') (shr2 y)

shrTmSub :: (Term -> [Term]) -> (Term -> [Term]) -> Term -> Maybe [Term]
shrTmSub shr1 shr2 = fmap shrTmSub' . preview _TmSub
  where
    shrTmSub' (x, y) =
      shr1 x ++
      shr2 y ++
      fmap (\x' -> TmSub x' y) (shr1 x) ++
      fmap (\y' -> TmSub x y') (shr2 y)

shrTmMul :: (Term -> [Term]) -> (Term -> [Term]) -> Term -> Maybe [Term]
shrTmMul shr1 shr2 = fmap shrTmMul' . preview _TmMul
  where
    shrTmMul' (x, y) =
      shr1 x ++
      shr2 y ++
      fmap (\x' -> TmMul x' y) (shr1 x) ++
      fmap (\y' -> TmMul x y') (shr2 y)

shrTmExp :: (Term -> [Term]) -> (Term -> [Term]) -> Term -> Maybe [Term]
shrTmExp shr1 shr2 = fmap shrTmExp' . preview _TmExp
  where
    shrTmExp' (x, y) =
      shr1 x ++
      shr2 y ++
      fmap (\x' -> TmExp x' y) (shr1 x) ++
      fmap (\y' -> TmExp x y') (shr2 y)

shrinkTerm :: Term -> [Term]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    shrTmInt
  , shrTmAdd shrinkTerm shrinkTerm
  , shrTmSub shrinkTerm shrinkTerm
  , shrTmMul shrinkTerm shrinkTerm
  , shrTmExp shrinkTerm shrinkTerm
  ]

-- TODO use pretty printer for the show here
newtype AnyTerm = AnyTerm { getAnyTerm :: Term }
                deriving (Eq, Ord, Show)

instance Arbitrary AnyTerm where
  arbitrary = AnyTerm <$> genTerm
  shrink = fmap AnyTerm . shrinkTerm . getAnyTerm
