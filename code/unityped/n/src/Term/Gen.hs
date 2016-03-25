module Term.Gen where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

import Term

genTmZero :: Gen Term
genTmZero = pure TmZero

genTmSucc :: Gen Term -> Gen Term
genTmSucc g = TmSucc <$> g

genTmPred :: Gen Term -> Gen Term
genTmPred g = TmPred <$> g

genTerm :: Gen Term
genTerm = sized genTerm'
  where
    genTerm' s =
      let
        zeroSize = [genTmZero]
        child = genTerm' (s `div` 2)
        nonZeroSize = [genTmSucc child, genTmPred child]
      in
        oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

shrTmZero :: Term -> Maybe [Term]
shrTmZero = fmap (const []) . preview _TmZero

shrTmSucc :: (Term -> [Term]) -> Term -> Maybe [Term]
shrTmSucc shr = fmap shrTmSucc' . preview _TmSucc
  where
    shrTmSucc' t =
      shr t ++
      fmap TmSucc (shr t)

shrTmPred :: (Term -> [Term]) -> Term -> Maybe [Term]
shrTmPred shr = fmap shrTmPred' . preview _TmPred
  where
    shrTmPred' t =
      shr t ++
      fmap TmPred (shr t)

shrinkTerm :: Term -> [Term]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    shrTmZero
  , shrTmSucc shrinkTerm
  , shrTmPred shrinkTerm
  ]

-- TODO use pretty printer for the show here
newtype AnyTerm = AnyTerm { getAnyTerm :: Term }
                deriving (Eq, Ord, Show)

instance Arbitrary AnyTerm where
  arbitrary = AnyTerm <$> genTerm
  shrink = fmap AnyTerm . shrinkTerm . getAnyTerm
