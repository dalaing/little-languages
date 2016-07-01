{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for terms of the I language.
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

-- | Generates 'TmInt' terms.
genTmInt :: Gen Term
genTmInt =
  TmInt <$> arbitrary

-- | Generates 'TmAdd' terms, given a generator for each of the subterms.
genTmAdd :: Gen Term -- ^ The generator for the first argument.
         -> Gen Term -- ^ The generator for the second argument.
         -> Gen Term
genTmAdd g1 g2 =
  TmAdd <$> g1 <*> g2

-- | Generates 'TmSub' terms, given a generator for each of the subterms.
genTmSub :: Gen Term -- ^ The generator for the first argument.
         -> Gen Term -- ^ The generator for the second argument.
         -> Gen Term
genTmSub g1 g2 =
  TmSub <$> g1 <*> g2

-- | Generates 'TmMul' terms, given a generator for each of the subterms.
genTmMul :: Gen Term -- ^ The generator for the first argument.
         -> Gen Term -- ^ The generator for the second argument.
         -> Gen Term
genTmMul g1 g2 =
  TmMul <$> g1 <*> g2

-- | Generates 'TmExp' terms, given a generator for each of the subterms.
genTmExp :: Gen Term -- ^ The generator for the first argument.
         -> Gen Term -- ^ The generator for the second argument.
         -> Gen Term
genTmExp g1 g2 =
  TmExp <$> g1 <*> g2

-- | Helper function for building the 'TmAdd' terms in 'genTerm'.
genTermTmAdd :: (Int -> Gen Term) -- ^ The generator for terms of the I language.
             -> Int
             -> Maybe (Gen Term)
genTermTmAdd _  0 =
  Nothing
genTermTmAdd gen s =
  let
    child = gen (s `div` 2)
  in
    Just $ genTmAdd child child

-- | Helper function for building the 'TmSub' terms in 'genTerm'.
genTermTmSub :: (Int -> Gen Term) -- ^ The generator for terms of the I language.
             -> Int
             -> Maybe (Gen Term)
genTermTmSub _  0 =
  Nothing
genTermTmSub gen s =
  let
    child = gen (s `div` 2)
  in
    Just $ genTmSub child child

-- | Helper function for building the 'TmMul' terms in 'genTerm'.
genTermTmMul :: (Int -> Gen Term) -- ^ The generator for terms of the I language.
             -> Int
             -> Maybe (Gen Term)
genTermTmMul _  0 =
  Nothing
genTermTmMul gen s =
  let
    child = gen (s `div` 2)
  in
    Just $ genTmMul child child

-- | Helper function for building the 'TmExp' terms in 'genTerm'.
genTermTmExp :: (Int -> Gen Term) -- ^ The generator for terms of the I language.
             -> Int
             -> Maybe (Gen Term)
genTermTmExp _  0 =
  Nothing
genTermTmExp gen s =
  let
    child = gen (s `div` 2)
  in
    Just $ genTmExp child child

-- | Generates terms of the I language.
--
-- The QuickCheck size parameter is interpreted as an upper bound on the
-- size of the term.
genTerm :: Gen Term
genTerm = sized genTerm'

-- | Helper function to generate terms of the I language with a specific size.
genTerm' :: Int
         -> Gen Term
genTerm' s =
  oneof $ [
      genTmInt
    ] ++ mapMaybe (\f -> f genTerm' s) [
      genTermTmAdd
    , genTermTmSub
    , genTermTmMul
    , genTermTmExp
    ]

-- | Shrinks 'TmInt' terms.
shrinkTmInt :: Term
            -> Maybe [Term]
shrinkTmInt (TmInt i) =
  Just (TmInt <$> shrink i)
shrinkTmInt _ =
  Nothing

-- | Shrinks 'TmAdd' terms
shrinkTmAdd :: (Term -> [Term]) -- ^ The shrinking function for terms of the I language.
            -> Term
            -> Maybe [Term]
shrinkTmAdd shr (TmAdd tm1 tm2) = Just $
  [tm1, tm2] ++
  fmap (\s -> TmAdd s tm2) (shr tm1) ++
  fmap (\s -> TmAdd tm1 s) (shr tm2)
shrinkTmAdd _ _ =
  Nothing

-- | Shrinks 'TmSub' terms
shrinkTmSub :: (Term -> [Term]) -- ^ The shrinking function for terms of the I language.
            -> Term
            -> Maybe [Term]
shrinkTmSub shr (TmSub tm1 tm2) = Just $
  [tm1, tm2] ++
  fmap (\s -> TmSub s tm2) (shr tm1) ++
  fmap (\s -> TmSub tm1 s) (shr tm2)
shrinkTmSub _ _ =
  Nothing

-- | Shrinks 'TmMul' terms
shrinkTmMul :: (Term -> [Term]) -- ^ The shrinking function for terms of the I language.
            -> Term
            -> Maybe [Term]
shrinkTmMul shr (TmMul tm1 tm2) = Just $
  [tm1, tm2] ++
  fmap (\s -> TmMul s tm2) (shr tm1) ++
  fmap (\s -> TmMul tm1 s) (shr tm2)
shrinkTmMul _ _ =
  Nothing

-- | Shrinks 'TmExp' terms
shrinkTmExp :: (Term -> [Term]) -- ^ The shrinking function for terms of the I language.
            -> Term
            -> Maybe [Term]
shrinkTmExp shr (TmExp tm1 tm2) = Just $
  [tm1, tm2] ++
  fmap (\s -> TmExp s tm2) (shr tm1) ++
  fmap (\s -> TmExp tm1 s) (shr tm2)
shrinkTmExp _ _ =
  Nothing
-- | The set of shrinking rules for terms of the I language.
shrinkTermRules :: [Term -> Maybe [Term]]
shrinkTermRules = [
    shrinkTmInt
  , shrinkTmAdd shrinkTerm
  , shrinkTmSub shrinkTerm
  , shrinkTmMul shrinkTerm
  , shrinkTmExp shrinkTerm
  ]

-- | Shrinks terms of the I language.
shrinkTerm :: Term
           -> [Term]
shrinkTerm tm =
  fromMaybe [] .
  asum .
  fmap ($ tm) $
  shrinkTermRules

-- | A newtype wrapper for generating terms of the I language.
newtype AnyTerm = AnyTerm {
    getAnyTerm :: Term
  } deriving (Eq, Show)

instance Arbitrary AnyTerm where
  arbitrary =
    fmap AnyTerm genTerm
  shrink =
    fmap AnyTerm . shrinkTerm . getAnyTerm
