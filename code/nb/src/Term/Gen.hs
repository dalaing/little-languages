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
  , AnyTerm(..)
  , genWellTypedTerm
  , shrinkWellTypedTerm
  , WellTypedTerm(..)
  , genContainingTerm
  , shrinkContainingTerm
  , ContainingTerm(..)
  , genIllTypedTerm
  , shrinkIllTypedTerm
  , IllTypedTerm(..)
  ) where

-- from 'base'
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe, mapMaybe)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

-- local
import           Term            (Term (..), contains)
import           Type            (Type (..))
import           Type.Gen        (genType)
import           Type.Error      (TypeError (..))
import           Type.Error.Gen  (genTypeError)

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

-- | Generates 'TmFalse' terms.
genTmFalse :: Gen Term
genTmFalse =
  pure TmFalse

-- | Generates 'TmTrue' terms.
genTmTrue :: Gen Term
genTmTrue =
  pure TmTrue

-- | Generates 'TmIf' terms, given a generator for each of the subterms.
genTmIf :: Gen Term -- ^ The generator for the test expression.
        -> Gen Term -- ^ The generator for the 'then' expression.
        -> Gen Term -- ^ The generator for the 'else' expression.
        -> Gen Term
genTmIf g1 g2 g3 =
  TmIf <$> g1 <*> g2 <*> g3

-- | Generates 'TmIsZero' terms, given a generator for the argument to 'TmIsZero.'
genTmIsZero :: Gen Term -- ^ The generator for the argument
            -> Gen Term
genTmIsZero g =
  TmIsZero <$> g

-- | Helper function for building the 'TmSucc' terms in 'genTerm'.
genTermTmSucc :: (Int -> Gen Term) -- ^ The generator for terms of the NB language.
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
genTermTmPred :: (Int -> Gen Term) -- ^ The generator for terms of the NB language.
              -> Int
              -> Maybe (Gen Term)
genTermTmPred _  0 =
  Nothing
genTermTmPred gen s =
  let
    child = gen (s - 1)
  in
    Just $ genTmPred child

-- | Helper function for building the 'TmIf' terms in 'genTerm'.
genTermTmIf :: (Int -> Gen Term) -- ^ The generator for terms of the NB language.
            -> Int
            -> Maybe (Gen Term)
genTermTmIf _  0 =
  Nothing
genTermTmIf gen s =
  let
    child = gen (s `div` 3)
  in
    Just $ genTmIf child child child

-- | Helper function for building the 'TmIsZero' terms in 'genTerm'.
genTermTmIsZero :: (Int -> Gen Term) -- ^ The generator for terms of the NB language.
                -> Int
                -> Maybe (Gen Term)
genTermTmIsZero _  0 =
  Nothing
genTermTmIsZero gen s =
  let
    child = gen (s - 1)
  in
    Just $ genTmIsZero child

-- | Generates terms of the NB language.
--
-- The QuickCheck size parameter is interpreted as an upper bound on the
-- size of the term.
genTerm :: Gen Term
genTerm = sized genTerm'

-- | Helper function to generate terms of the NB language with a specific size.
genTerm' :: Int
         -> Gen Term
genTerm' s =
  oneof $ [
      genTmZero
    , genTmFalse
    , genTmTrue
    ] ++ mapMaybe (\f -> f genTerm' s) [
      genTermTmSucc
    , genTermTmPred
    , genTermTmIf
    , genTermTmIsZero
    ]

-- | Shrinks 'TmZero' terms.
shrinkTmZero :: Term
             -> Maybe [Term]
shrinkTmZero TmZero =
  Just []
shrinkTmZero _ =
  Nothing

-- | Shrinks 'TmSucc' terms.
shrinkTmSucc :: (Term -> [Term]) -- ^ The shrinking function for terms of the NB language.
             -> Term
             -> Maybe [Term]
shrinkTmSucc shr (TmSucc tm) =
  Just $ tm : fmap TmSucc (shr tm)
shrinkTmSucc _ _ =
  Nothing

-- | Shrinks 'TmPred' terms.
shrinkTmPred :: (Term -> [Term]) -- ^ The shrinking function for terms of the NB language.
             -> Term
             -> Maybe [Term]
shrinkTmPred shr (TmPred tm) =
  Just $ tm : fmap TmPred (shr tm)
shrinkTmPred _ _ =
  Nothing

-- | Shrinks 'TmFalse' terms.
shrinkTmFalse :: Term
              -> Maybe [Term]
shrinkTmFalse TmFalse =
  Just []
shrinkTmFalse _ =
  Nothing

-- | Shrinks 'TmTrue' terms.
shrinkTmTrue :: Term
             -> Maybe [Term]
shrinkTmTrue TmTrue =
  Just []
shrinkTmTrue _ =
  Nothing

-- | Shrinks 'TmIf' terms.
shrinkTmIf :: (Term -> [Term]) -- ^ The shrinking function for terms of the NB language.
           -> Term
           -> Maybe [Term]
shrinkTmIf shr (TmIf tm1 tm2 tm3) = Just $
  [tm1, tm2, tm3] ++
  fmap (\tm1' -> TmIf tm1' tm2 tm3) (shr tm1) ++
  fmap (\tm2' -> TmIf tm1 tm2' tm3) (shr tm2) ++
  fmap (\tm3' -> TmIf tm1 tm2 tm3') (shr tm3)
shrinkTmIf _ _ =
  Nothing

-- | Shrinks 'TmIsZero' terms.
shrinkTmIsZero :: (Term -> [Term]) -- ^ The shrinking function for terms of the NB language.
               -> Term
               -> Maybe [Term]
shrinkTmIsZero shr (TmIsZero tm) =
  Just $ tm : fmap TmIsZero (shr tm)
shrinkTmIsZero _ _ =
  Nothing

-- | The set of shrinking rules for terms of the NB language.
shrinkTermRules :: [Term -> Maybe [Term]]
shrinkTermRules = [
    shrinkTmZero
  , shrinkTmSucc shrinkTerm
  , shrinkTmPred shrinkTerm
  , shrinkTmFalse
  , shrinkTmTrue
  , shrinkTmIf shrinkTerm
  , shrinkTmIsZero shrinkTerm
  ]

-- | Shrinks terms of the NB language.
shrinkTerm :: Term
           -> [Term]
shrinkTerm tm =
  fromMaybe [] .
  asum .
  fmap ($ tm) $
  shrinkTermRules

-- | A newtype wrapper for generating terms of the NB language.
newtype AnyTerm = AnyTerm {
    getAnyTerm :: Term
  } deriving (Eq, Show)

instance Arbitrary AnyTerm where
  arbitrary =
    fmap AnyTerm genTerm
  shrink =
    fmap AnyTerm . shrinkTerm . getAnyTerm

-- | Generates well-typed 'TmZero' terms.
genWellTypedTmZero :: Type
                   -> Maybe (Gen Term)
genWellTypedTmZero TyNat =
  Just genTmZero
genWellTypedTmZero _ =
  Nothing

-- | Generates well-typed 'TmFalse' terms.
genWellTypedTmFalse :: Type
                    -> Maybe (Gen Term)
genWellTypedTmFalse TyBool =
  Just genTmFalse
genWellTypedTmFalse _ =
  Nothing

-- | Generates well-typed 'TmTrue' terms.
genWellTypedTmTrue :: Type
                   -> Maybe (Gen Term)
genWellTypedTmTrue TyBool =
  Just genTmTrue
genWellTypedTmTrue _ =
  Nothing

-- | Generates well-typed 'TmSucc' terms.
genWellTypedTmSucc :: Type
                   -> (Type -> Int -> Gen Term) -- ^ The function for generating well-typed terms of the NB language.
                   -> Int
                   -> Maybe (Gen Term)
genWellTypedTmSucc _ _ 0 =
  Nothing
genWellTypedTmSucc TyNat gen s =
  let
    child = gen TyNat (s - 1)
  in
    Just $ genTmSucc child
genWellTypedTmSucc _ _ _ =
  Nothing

-- | Generates well-typed 'TmPred' terms.
genWellTypedTmPred :: Type
                   -> (Type -> Int -> Gen Term) -- ^ The function for generating well-typed terms of the NB language.
                   -> Int
                   -> Maybe (Gen Term)
genWellTypedTmPred _ _ 0 =
  Nothing
genWellTypedTmPred TyNat gen s =
  let
    child = gen TyNat (s - 1)
  in
    Just $ genTmPred child
genWellTypedTmPred _ _ _ =
  Nothing

-- | Generates well-typed 'TmIf' terms.
genWellTypedTmIf :: Type
                 -> (Type -> Int -> Gen Term) -- ^ The function for generating well-typed terms of the NB language.
                 -> Int
                 -> Maybe (Gen Term)
genWellTypedTmIf _ _ 0 =
  Nothing
genWellTypedTmIf ty gen s =
  let
    childB = gen TyBool (s `div` 3)
    child  = gen ty (s `div` 3)
  in
    Just $ genTmIf childB child child

-- | Generates well-typed 'TmIsZero' terms.
genWellTypedTmIsZero :: Type
                     -> (Type -> Int -> Gen Term) -- ^ The function for generating well-typed terms of the NB language.
                     -> Int
                     -> Maybe (Gen Term)
genWellTypedTmIsZero _ _ 0 =
  Nothing
genWellTypedTmIsZero TyBool gen s =
  let
    child = gen TyNat (s - 1)
  in
    Just $ genTmIsZero child
genWellTypedTmIsZero _ _ _ =
  Nothing

-- | Generates well-typed terms of the NB language.
--
-- The QuickCheck size parameter is interpreted as an upper bound on the
-- size of the term.
genWellTypedTerm :: Type
                 -> Gen Term
genWellTypedTerm ty =
   sized (genWellTypedTerm' ty)

-- | Helper function to generate terms of the NB language with a specific size.
genWellTypedTerm' :: Type
                  -> Int
                  -> Gen Term
genWellTypedTerm' ty s =
  oneof $ mapMaybe (\f -> f ty) [
      genWellTypedTmZero
    , genWellTypedTmFalse
    , genWellTypedTmTrue
    ] ++ mapMaybe (\f -> f ty genWellTypedTerm' s) [
      genWellTypedTmSucc
    , genWellTypedTmPred
    , genWellTypedTmIf
    , genWellTypedTmIsZero
    ]

-- | Shrinks well-typed 'TmIf' terms.
shrinkWellTypedTmIf :: (Term -> [Term]) -- ^ The shrinking function for well-typed terms of the NB language.
                    -> Term
                    -> Maybe [Term]
shrinkWellTypedTmIf shr (TmIf tm1 tm2 tm3) = Just $
  -- can put this back if/when we have annotations for how this things were generated
  -- and we know that the TmIf was generated as part of a well-typed term of type Bool
  -- [tm1, tm2, tm3] ++
  [tm2, tm3] ++
  fmap (\tm1' -> TmIf tm1' tm2 tm3) (shr tm1)
shrinkWellTypedTmIf _ _ =
  Nothing

-- | Shrinks well-typed 'TmIsZero' terms.
shrinkWellTypedTmIsZero :: (Term -> [Term]) -- ^ The shrinking function for well-typed terms of the NB language.
                        -> Term
                        -> Maybe [Term]
shrinkWellTypedTmIsZero shr (TmIsZero tm) =
  Just $ fmap TmIsZero (shr tm)
shrinkWellTypedTmIsZero _ _ =
  Nothing

-- | The set of shrinking rules for well-typed terms of the NB language.
shrinkWellTypedTermRules :: [Term -> Maybe [Term]]
shrinkWellTypedTermRules = [
    shrinkTmZero
  , shrinkTmSucc shrinkWellTypedTerm
  , shrinkTmPred shrinkWellTypedTerm
  , shrinkTmFalse
  , shrinkTmTrue
  , shrinkWellTypedTmIf shrinkWellTypedTerm
  , shrinkWellTypedTmIsZero shrinkWellTypedTerm
  ]

-- | Shrinks well-typed terms of the NB language.
shrinkWellTypedTerm :: Term
                    -> [Term]
shrinkWellTypedTerm tm =
  fromMaybe [] .
  asum .
  fmap ($ tm) $
  shrinkWellTypedTermRules

-- | A newtype wrapper for generating well-typed terms of the NB language.
newtype WellTypedTerm = WellTypedTerm {
    getWellTypedTerm :: Term
  } deriving (Eq, Ord, Show)

instance Arbitrary WellTypedTerm where
  arbitrary = do
    ty <- genType
    tm <- genWellTypedTerm ty
    return $ WellTypedTerm tm
  shrink =
    fmap WellTypedTerm .
    shrinkWellTypedTerm .
    getWellTypedTerm

-- |
genContainingTmZero :: Type
                    -> Term
                    -> Type
                    -> Maybe (Gen Term)
genContainingTmZero TyNat TmZero TyNat =
  Just genTmZero
genContainingTmZero _ _ _ =
  Nothing

-- |
genContainingTmSucc :: Type
                    -> Term
                    -> Type
                    -> (Type -> Term -> Type -> Int -> Gen Term)
                    -> Int
                    -> Maybe (Gen Term)
genContainingTmSucc TyNat tm tyO genContaining s =
  Just $ genContaining TyNat (TmSucc tm) tyO (s - 1 `max` 0)
genContainingTmSucc _ _ _ _ _ =
  Nothing

-- |
genContainingTmPred :: Type
                    -> Term
                    -> Type
                    -> (Type -> Term -> Type -> Int -> Gen Term)
                    -> Int
                    -> Maybe (Gen Term)
genContainingTmPred TyNat tm tyO genContaining s =
  Just $ genContaining TyNat (TmPred tm) tyO (s - 1 `max` 0)
genContainingTmPred _ _ _ _ _ =
  Nothing

-- |
genContainingTmFalse :: Type
                     -> Term
                     -> Type
                     -> Maybe (Gen Term)
genContainingTmFalse TyBool TmFalse TyBool =
  Just genTmFalse
genContainingTmFalse _ _ _ =
  Nothing

-- |
genContainingTmTrue :: Type
                    -> Term
                    -> Type
                    -> Maybe (Gen Term)
genContainingTmTrue TyBool TmTrue TyBool =
  Just genTmTrue
genContainingTmTrue _ _ _ =
  Nothing

-- |
genContainingTmIfTest :: Type
                      -> Term
                      -> Type
                      -> (Type -> Term -> Type -> Int -> Gen Term)
                      -> (Type -> Int -> Gen Term)
                      -> Int
                      -> Maybe (Gen Term)
genContainingTmIfTest tyI tm tyO genContaining genWellTyped s = Just $
  let
    childSize = s `div` 3
    genTest = genContaining tyI tm TyBool childSize
    genThen = genWellTyped tyO childSize
    genElse = genWellTyped tyO childSize
  in
    genTmIf genTest genThen genElse

-- |
genContainingTmIfThen :: Type
                      -> Term
                      -> Type
                      -> (Type -> Term -> Type -> Int -> Gen Term)
                      -> (Type -> Int -> Gen Term)
                      -> Int
                      -> Maybe (Gen Term)
genContainingTmIfThen tyI tm tyO genContaining genWellTyped s = Just $
  let
    childSize = s `div` 3
    genTest = genWellTyped TyBool childSize
    genThen = genContaining tyI tm tyO childSize
    genElse = genWellTyped tyO childSize
  in
    genTmIf genTest genThen genElse

-- |
genContainingTmIfElse :: Type
                      -> Term
                      -> Type
                      -> (Type -> Term -> Type -> Int -> Gen Term)
                      -> (Type -> Int -> Gen Term)
                      -> Int
                      -> Maybe (Gen Term)
genContainingTmIfElse tyI tm tyO genContaining genWellTyped s = Just $
  let
    childSize = s `div` 3
    genTest = genWellTyped TyBool childSize
    genThen = genWellTyped tyO childSize
    genElse = genContaining tyI tm tyO childSize
  in
    genTmIf genTest genThen genElse

-- |
genContainingTmIsZero :: Type
                      -> Term
                      -> Type
                      -> (Type -> Term -> Type -> Int -> Gen Term)
                      -> Int
                      -> Maybe (Gen Term)
genContainingTmIsZero TyNat tm tyO genContaining s =
  Just $ genContaining TyBool (TmIsZero tm) tyO (s - 1 `max` 0)
genContainingTmIsZero _ _ _ _ _ =
  Nothing

-- |
genContainingTerm :: Type
                  -> Term
                  -> Type
                  -> Gen Term
genContainingTerm tyI tmI tyO =
  sized (genContainingTerm' tyI tmI tyO)

-- |
genContainingTerm' :: Type
                   -> Term
                   -> Type
                   -> Int
                   -> Gen Term
genContainingTerm' tyI tmI tyO s =
  oneof $
    if s == 0 then [pure tmI] else [] ++
    mapMaybe (\f -> f tyI tmI tyO) [
        genContainingTmZero
      , genContainingTmFalse
      , genContainingTmTrue
      ] ++
    mapMaybe (\f -> f tyI tmI tyO genContainingTerm' s) [
        genContainingTmSucc
      , genContainingTmPred
      , genContainingTmIsZero
      ] ++
    mapMaybe (\f -> f tyI tmI tyO genContainingTerm' genWellTypedTerm' s) [
        genContainingTmIfTest
      , genContainingTmIfThen
      , genContainingTmIfElse
      ]

-- |

-- we need annotations before we can do more here
shrinkContainingTerm :: Term
                     -> [Term]
shrinkContainingTerm _ =
  []

-- |
data ContainingTerm = ContainingTerm {
    outerTerm :: Term
  , innerTerm :: Term
  } deriving (Eq, Ord, Show)

instance Arbitrary ContainingTerm where
  arbitrary = do
    tyI <- genType
    i <- genWellTypedTerm tyI
    tyO <- genType
    o <- genContainingTerm tyI i tyO
    return $ ContainingTerm o i

  shrink (ContainingTerm o i) =
    fmap (\o' -> ContainingTerm o' i).
    filter (`contains` i).
    shrinkTerm $
    o

-- | Generate a 'TmSucc' term that will cause a given type error, if possible.
genIllTypedTmSucc :: TypeError
                  -> Type
                  -> (Type -> Int -> Gen Term) -- ^ The function for generating well-typed terms of the NB language.
                  -> Int
                  -> Maybe (Gen Term)
genIllTypedTmSucc (Unexpected ty TyNat) TyNat gen s = Just $
  genTmSucc (gen ty s)
genIllTypedTmSucc _ _ _ _ =
  Nothing

-- | Generate a 'TmPred' term that will cause a given type error, if possible.
genIllTypedTmPred :: TypeError
                  -> Type
                  -> (Type -> Int -> Gen Term) -- ^ The function for generating well-typed terms of the NB language.
                  -> Int
                  -> Maybe (Gen Term)
genIllTypedTmPred (Unexpected ty TyNat) TyNat gen s = Just $
  genTmPred (gen ty s)
genIllTypedTmPred _ _ _ _ =
  Nothing

-- | Generate a 'TmIf' term that will cause a given type error, if possible.
genIllTypedTmIf :: TypeError
                -> Type
                -> (Type -> Int -> Gen Term) -- ^ The function for generating well-typed terms of the NB language.
                -> Int
                -> Maybe (Gen Term)
genIllTypedTmIf (ExpectedEq ty1 ty2) _ gen s = Just $
  let
    size = s `div` 3
    childB = gen TyBool size
    child1 = gen ty1 size
    child2 = gen ty2 size
  in
    genTmIf childB child1 child2
genIllTypedTmIf (Unexpected tyB TyBool) ty gen s = Just $ do
  let
    size = s `div` 3
    childB = gen tyB size
    child = gen ty size
  genTmIf childB child child
genIllTypedTmIf _ _ _ _ =
  Nothing

-- | Generate a 'TmIsZero' term that will cause a given type error, if possible.
genIllTypedTmIsZero :: TypeError
                    -> Type
                    -> (Type -> Int -> Gen Term) -- ^ The function for generating well-typed terms of the NB language.
                    -> Int
                    -> Maybe (Gen Term)
genIllTypedTmIsZero (Unexpected ty TyNat) TyBool gen s = Just $
  genTmIsZero (gen ty s)
genIllTypedTmIsZero _ _ _ _ =
  Nothing

-- |
--
-- Where possible, the term will have 
genIllTypedTerm :: TypeError
                -> Type
                -> Gen Term
genIllTypedTerm te ty =
   sized (genIllTypedTerm' te ty)

-- |
genIllTypedTerm' :: TypeError
                 -> Type
                 -> Int
                 -> Gen Term
genIllTypedTerm' te ty s =
  oneof .
  mapMaybe (\f -> f te ty genWellTypedTerm' s) $ [
      genIllTypedTmSucc
    , genIllTypedTmPred
    , genIllTypedTmIf
    , genIllTypedTmIsZero
    ]

-- |

-- we need annotations before we can do more here
shrinkIllTypedTerm :: Term
                   -> [Term]
shrinkIllTypedTerm _ =
  []

-- |
newtype IllTypedTerm = IllTypedTerm {
    getIllTypedTerm :: Term
  } deriving (Eq, Ord, Show)

instance Arbitrary IllTypedTerm where
  arbitrary = do
    te <- genTypeError
    ty <- genType
    tm <- genIllTypedTerm te ty
    tyO <- genType
    tmO <- genContainingTerm ty tm tyO
    return $ IllTypedTerm tmO
  shrink =
    fmap IllTypedTerm .
    shrinkIllTypedTerm .
    getIllTypedTerm
