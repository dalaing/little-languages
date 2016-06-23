module Term.Gen where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

import Term

genTmFalse :: Gen Term
genTmFalse = pure TmFalse

genTmTrue :: Gen Term
genTmTrue = pure TmTrue

genTmIf :: Gen Term -> Gen Term -> Gen Term -> Gen Term
genTmIf g1 g2 g3 = TmIf <$> g1 <*> g2 <*> g3

genTerm :: Gen Term
genTerm = sized genTerm'
  where
    genTerm' s =
      let
        zeroSize = [genTmFalse, genTmTrue]
        child = genTerm' (s `div` 2)
        nonZeroSize = [genTmIf child child child]
      in
        oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

shrTmFalse :: Term -> Maybe [Term]
shrTmFalse = fmap (const []) . preview _TmFalse

shrTmTrue :: Term -> Maybe [Term]
shrTmTrue = fmap (const []) . preview _TmTrue

shrTmIf :: (Term -> [Term]) -> (Term -> [Term]) -> (Term -> [Term]) -> Term -> Maybe [Term]
shrTmIf s1 s2 s3 = fmap shrTmIf' . preview _TmIf
  where
    shrTmIf' (t1, t2, t3) =
      s2 t2 ++ 
      s3 t3 ++
      fmap (\u1 -> TmIf u1 t2 t3) (s1 t1) ++
      fmap (\u2 -> TmIf t1 u2 t3) (s2 t2) ++
      fmap (\u3 -> TmIf t1 t2 u3) (s3 t3)

shrinkTerm :: Term -> [Term]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    shrTmFalse
  , shrTmTrue
  , shrTmIf shrinkTerm shrinkTerm shrinkTerm
  ]

-- TODO use pretty printer for the show here
newtype AnyTerm = AnyTerm { getAnyTerm :: Term }
                deriving (Eq, Ord, Show)

instance Arbitrary AnyTerm where
  arbitrary = AnyTerm <$> genTerm
  shrink = fmap AnyTerm . shrinkTerm . getAnyTerm
