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
  ) where

-- from 'base'
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

-- local
import           Term            (Term (..))

-- | Generates 'TmInt' terms.
genTmInt :: Gen Term
genTmInt =
  TmInt <$> arbitrary

-- | Shrinks 'TmInt' terms.
shrinkTmInt :: Term
            -> Maybe [Term]
shrinkTmInt (TmInt i) =
  Just (TmInt <$> shrink i)
shrinkTmInt _ =
  Nothing

-- | Generates 'TmAdd' terms, given a generator for each of the subterms.
genTmAdd :: Gen Term -- ^ The generator for the first argument.
         -> Gen Term -- ^ The generator for the second argument.
         -> Gen Term
genTmAdd g1 g2 =
  TmAdd <$> g1 <*> g2

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

-- | Generates 'TmSub' terms, given a generator for each of the subterms.
genTmSub :: Gen Term -- ^ The generator for the first argument.
         -> Gen Term -- ^ The generator for the second argument.
         -> Gen Term
genTmSub g1 g2 =
  TmSub <$> g1 <*> g2

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

-- | Generates 'TmMul' terms, given a generator for each of the subterms.
genTmMul :: Gen Term -- ^ The generator for the first argument.
         -> Gen Term -- ^ The generator for the second argument.
         -> Gen Term
genTmMul g1 g2 =
  TmMul <$> g1 <*> g2

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

-- | Generates 'TmExp' terms, given a generator for each of the subterms.
genTmExp :: Gen Term -- ^ The generator for the first argument.
         -> Gen Term -- ^ The generator for the second argument.
         -> Gen Term
genTmExp g1 g2 =
  TmExp <$> g1 <*> g2

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

-- | Generates terms of the I language.
--
-- The QuickCheck size parameter is interpreted as an upper bound on the
-- size of the term.
genTerm :: Gen Term
genTerm = sized genTerm'

-- | Helper function to generate terms of the I language with a specific size.
genTerm' :: Int
         -> Gen Term
genTerm' 0 =
    genTmInt
genTerm' s =
    oneof
      [ genTmInt
      , genTmAdd child2 child2
      , genTmSub child2 child2
      , genTmMul child2 child2
      , genTmExp child2 child2
      ]
  where
    s2 = s `div` 2
    child2 = genTerm' s2

-- | Shrinks terms of the I language.
shrinkTerm :: Term
           -> [Term]
shrinkTerm tm =
  fromMaybe [] .
  asum .
  fmap ($ tm) $
    [ shrinkTmInt
    , shrinkTmAdd shrinkTerm
    , shrinkTmSub shrinkTerm
    , shrinkTmMul shrinkTerm
    , shrinkTmExp shrinkTerm
    ]
