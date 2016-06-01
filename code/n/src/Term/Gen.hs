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
shrinkTmSucc :: (Term -> [Term]) -- ^ The shrinking function for terms of the N language.
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
shrinkTmPred :: (Term -> [Term]) -- ^ The shrinking function for terms of the N language.
             -> Term
             -> Maybe [Term]
shrinkTmPred shrink (TmPred tm) =
  Just $ tm : fmap TmPred (shrink tm)
shrinkTmPred _ _ =
  Nothing

-- | Generates terms of the N language.
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
    ]
genTerm' s =
    oneof
      [ genTmZero
      , genTmSucc child
      , genTmPred child
      ]
  where
    s' = s - 1
    child = genTerm' s'

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
    ]
