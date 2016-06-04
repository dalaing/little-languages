{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for terms of the NB language.
-}
module Term.Gen (
    genTerm
  , shrinkTerm
  ) where

-- from 'base'
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, oneof, sized)

-- local
import           Term            (Term (..))

-- | Generates 'TmZero' terms.
genTmZero :: Gen Term
genTmZero =
  pure TmZero

-- | Shrinks 'TmZero' terms.
shrinkTmZero :: Term
             -> Maybe [Term]
shrinkTmZero TmZero =
  Just []
shrinkTmZero _ =
  Nothing

-- | Generates 'TmSucc' terms, given a generator for the argument to 'TmSucc.'
genTmSucc :: Gen Term -- ^ The generator for the argument
          -> Gen Term
genTmSucc g =
  TmSucc <$> g

-- | Shrinks 'TmSucc' terms.
shrinkTmSucc :: (Term -> [Term]) -- ^ The shrinking function for terms of the NB language.
             -> Term
             -> Maybe [Term]
shrinkTmSucc shrink (TmSucc tm) =
  Just $ tm : fmap TmSucc (shrink tm)
shrinkTmSucc _ _ =
  Nothing

-- | Generates 'TmPred' terms, given a generator for the argument to 'TmPred.'
genTmPred :: Gen Term -- ^ The generator for the argument
          -> Gen Term
genTmPred g =
  TmPred <$> g

-- | Shrinks 'TmPred' terms.
shrinkTmPred :: (Term -> [Term]) -- ^ The shrinking function for terms of the NB language.
             -> Term
             -> Maybe [Term]
shrinkTmPred shrink (TmPred tm) =
  Just $ tm : fmap TmPred (shrink tm)
shrinkTmPred _ _ =
  Nothing

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
shrinkTmIf :: (Term -> [Term]) -- ^ The shrinking function for terms of the NB language.
           -> Term
           -> Maybe [Term]
shrinkTmIf shrink (TmIf tm1 tm2 tm3) = Just $
  [tm1, tm2, tm3] ++
  fmap (\tm1' -> TmIf tm1' tm2 tm3) (shrink tm1) ++
  fmap (\tm2' -> TmIf tm1 tm2' tm3) (shrink tm2) ++
  fmap (\tm3' -> TmIf tm1 tm2 tm3') (shrink tm3)
shrinkTmIf _ _ =
  Nothing

-- | Generates 'TmIsZero' terms, given a generator for the argument to 'TmIsZero.'
genTmIsZero :: Gen Term -- ^ The generator for the argument
            -> Gen Term
genTmIsZero g =
  TmIsZero <$> g

-- | Shrinks 'TmIsZero' terms.
shrinkTmIsZero :: (Term -> [Term]) -- ^ The shrinking function for terms of the NB language.
               -> Term
               -> Maybe [Term]
shrinkTmIsZero shrink (TmIsZero tm) =
  Just $ tm : fmap TmIsZero (shrink tm)
shrinkTmIsZero _ _ =
  Nothing

-- | Generates terms of the NB language.
--
-- The QuickCheck size parameter is interpreted as an upper bound on the
-- size of the term.
genTerm :: Gen Term
genTerm = sized genTerm'

-- | Helper function to generate terms of the N language with a specific size.
genTerm' :: Int
         -> Gen Term
genTerm' 0 =
  oneof
    [ genTmZero
    , genTmFalse
    , genTmTrue
    ]
genTerm' s =
    oneof
      [ genTmZero
      , genTmFalse
      , genTmTrue
      , genTmSucc child1
      , genTmPred child1
      , genTmIf child3 child3 child3
      , genTmIsZero child1
      ]
  where
    s1 = s - 1
    child1 = genTerm' s1
    s3 = s `div` 3
    child3 = genTerm' s3

-- | Shrinks terms of the N language.
shrinkTerm :: Term
           -> [Term]
shrinkTerm tm =
  fromMaybe [] .
  asum .
  fmap ($ tm) $
    [ shrinkTmZero
    , shrinkTmSucc shrinkTerm
    , shrinkTmPred shrinkTerm
    , shrinkTmFalse
    , shrinkTmTrue
    , shrinkTmIf shrinkTerm
    , shrinkTmIsZero shrinkTerm
    ]
