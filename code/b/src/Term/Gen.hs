{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for terms of the B language.
-}
module Term.Gen (
    genTerm
  , shrinkTerm
  , AnyTerm(..)
  ) where

-- from 'base'
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

-- local
import           Term            (Term (..))

-- | Generates 'TmFalse' terms.
genTmFalse :: Gen Term
genTmFalse =
  pure TmFalse

-- | Shrinks 'TmFalse' terms.
shrinkTmFalse :: Term
              -> Maybe [Term]
shrinkTmFalse TmFalse =
  Just []
shrinkTmFalse _ =
  Nothing

-- | Generates 'TmTrue' terms.
genTmTrue :: Gen Term
genTmTrue =
  pure TmTrue

-- | Shrinks 'TmTrue' terms.
shrinkTmTrue :: Term
             -> Maybe [Term]
shrinkTmTrue TmTrue =
  Just []
shrinkTmTrue _ =
  Nothing

-- | Generates 'TmIf' terms, given a generator for each of the subterms.
genTmIf :: Gen Term -- ^ The generator for the test expression.
        -> Gen Term -- ^ The generator for the 'then' expression.
        -> Gen Term -- ^ The generator for the 'else' expression.
        -> Gen Term
genTmIf g1 g2 g3 =
  TmIf <$> g1 <*> g2 <*> g3

-- | Shrinks 'TmIf' terms.
shrinkTmIf :: (Term -> [Term]) -- ^ The shrinking function for terms of the B language.
           -> Term
           -> Maybe [Term]
shrinkTmIf shr (TmIf tm1 tm2 tm3) = Just $
  [tm1, tm2, tm3] ++
  fmap (\tm1' -> TmIf tm1' tm2 tm3) (shr tm1) ++
  fmap (\tm2' -> TmIf tm1 tm2' tm3) (shr tm2) ++
  fmap (\tm3' -> TmIf tm1 tm2 tm3') (shr tm3)
shrinkTmIf _ _ =
  Nothing

-- | Generates terms of the B language.
--
-- The QuickCheck size parameter is interpreted as an upper bound on the
-- size of the term.
genTerm :: Gen Term
genTerm = sized genTerm'

-- | Helper function to generate terms of the B language with a specific size.
genTerm' :: Int
         -> Gen Term
genTerm' 0 =
  oneof
    [ genTmFalse
    , genTmTrue
    ]
genTerm' s =
    oneof
      [ genTmFalse
      , genTmTrue
      , genTmIf child3 child3 child3
      ]
  where
    s3 = s `div` 3
    child3 = genTerm' s3

-- | The set of shrinking rules for terms of the B language.
shrinkTermRules :: [Term -> Maybe [Term]]
shrinkTermRules = [
    shrinkTmFalse
  , shrinkTmTrue
  , shrinkTmIf shrinkTerm
  ]

-- | The shrinking function for terms of the B language.
shrinkTerm :: Term
           -> [Term]
shrinkTerm tm =
  fromMaybe [] .
  asum .
  fmap ($ tm) $
  shrinkTermRules

-- | A newtype wrapped for generating terms of the B language.
newtype AnyTerm = AnyTerm {
    getAnyTerm :: Term
  } deriving (Eq, Show)

instance Arbitrary AnyTerm where
  arbitrary =
    fmap AnyTerm genTerm
  shrink =
    fmap AnyTerm . shrinkTerm . getAnyTerm
