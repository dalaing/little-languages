{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for terms of the N language.
-}
module Term.Gen (
    genTerm
  , shrinkTerm
  , AnyTerm(..)
  ) where

-- from 'base'
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe, mapMaybe)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

-- local
import           Term            (Term (..))

-- | Generates 'TmZero' terms.
genTmZero :: Gen Term
genTmZero =
  pure TmZero

-- | Generates 'TmSucc' terms, given a generator for the argument to 'TmSucc.'
genTmSucc :: Gen Term -- ^ The generator for the argument
          -> Gen Term
genTmSucc g =
  TmSucc <$> g

-- | Generates 'TmPred' terms, given a generator for the argument to 'TmPred.'
genTmPred :: Gen Term -- ^ The generator for the argument
          -> Gen Term
genTmPred g =
  TmPred <$> g

-- | Helper function for building the 'TmSucc' terms in 'genTerm'.
genTermTmSucc :: (Int -> Gen Term) -- ^ The generator for terms of the N language.
              -> Int
              -> Maybe (Gen Term)
genTermTmSucc _  0 =
  Nothing
genTermTmSucc gen s =
  let
    child = gen (s - 1)
  in
    Just $ genTmSucc child

-- | Helper function for building the 'TmPred' terms in 'genTerm'.
genTermTmPred :: (Int -> Gen Term) -- ^ The generator for terms of the N language.
              -> Int
              -> Maybe (Gen Term)
genTermTmPred _  0 =
  Nothing
genTermTmPred gen s =
  let
    child = gen (s - 1)
  in
    Just $ genTmPred child

-- | Generates terms of the N language.
--
-- The QuickCheck size parameter is interpreted as an upper bound on the
-- size of the term.
genTerm :: Gen Term
genTerm = sized genTerm'

-- | Helper function to generate terms of the N language with a specific size.
genTerm' :: Int
         -> Gen Term
genTerm' s =
  oneof $ [
      genTmZero
    ] ++ mapMaybe (\f -> f genTerm' s) [
      genTermTmSucc
    , genTermTmPred
    ]

-- | Shrinks 'TmZero' terms.
shrinkTmZero :: Term
             -> Maybe [Term]
shrinkTmZero TmZero =
  Just []
shrinkTmZero _ =
  Nothing

-- | Shrinks 'TmSucc' terms.
shrinkTmSucc :: (Term -> [Term]) -- ^ The shrinking function for terms of the N language.
             -> Term
             -> Maybe [Term]
shrinkTmSucc shr (TmSucc tm) =
  Just $ tm : fmap TmSucc (shr tm)
shrinkTmSucc _ _ =
  Nothing

-- | Shrinks 'TmPred' terms.
shrinkTmPred :: (Term -> [Term]) -- ^ The shrinking function for terms of the N language.
             -> Term
             -> Maybe [Term]
shrinkTmPred shr (TmPred tm) =
  Just $ tm : fmap TmPred (shr tm)
shrinkTmPred _ _ =
  Nothing

-- | The set of shrinking rules for terms of the N language.
shrinkTermRules :: [Term -> Maybe [Term]]
shrinkTermRules = [
    shrinkTmZero
  , shrinkTmSucc shrinkTerm
  , shrinkTmPred shrinkTerm
  ]

-- | Shrinks terms of the N language.
shrinkTerm :: Term
           -> [Term]
shrinkTerm tm =
  fromMaybe [] .
  asum .
  fmap ($ tm) $
  shrinkTermRules

-- | A newtype wrapped for generating terms of the N language.
newtype AnyTerm = AnyTerm {
    getAnyTerm :: Term
  } deriving (Eq, Show)

instance Arbitrary AnyTerm where
  arbitrary =
    fmap AnyTerm genTerm
  shrink =
    fmap AnyTerm . shrinkTerm . getAnyTerm
