{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Rules (
    TypeErrorPrettyRule(..)
  , TypeErrorRules(..)
  , TypeGenRule(..)
  , TypeParseRule(..)
  , TypePrettyRule(..)
  , TypeRules(..)
  , TermGenRule(..)
  , TermParseRule(..)
  , TermPrettyRule(..)
  , ValueRule(..)
  , SmallStepRule(..)
  , BigStepRule(..)
  , InferRule(..)
  , TermRules(..)
  , LanguageRules(..)
  ) where

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen)

data TypeErrorPrettyRule ty te =
  TypeErrorPrettyBase ((ty -> Doc) -> te -> Maybe Doc)

data TypeErrorRules ty te = TypeErrorRules {
    tePrettyRules :: [TypeErrorPrettyRule ty te]
  }

instance Monoid (TypeErrorRules ty te) where
  mempty =
    TypeErrorRules
      mempty
  mappend te1 te2 =
    TypeErrorRules
      (mappend (tePrettyRules te1) (tePrettyRules te2))

data TypeGenRule ty =
    TypeGenBase (Gen ty)

data TypeParseRule m ty =
  TypeParseBase (m ty)

data TypePrettyRule ty =
  TypePrettyBase (ty -> Maybe Doc)

data TypeRules m ty = TypeRules {
    tyGenRules    :: [TypeGenRule ty]
  , tyParseRules  :: [TypeParseRule m ty]
  , tyPrettyRules :: [TypePrettyRule ty]
  }

instance Monoid (TypeRules m ty) where
  mempty =
    TypeRules
      mempty
      mempty
      mempty
  mappend ty1 ty2 =
    TypeRules
      (mappend (tyGenRules ty1) (tyGenRules ty2))
      (mappend (tyParseRules ty1) (tyParseRules ty2))
      (mappend (tyPrettyRules ty1) (tyPrettyRules ty2))

data TermGenRule tm =
    TermGenBase (Gen tm)
  | TermGenRecurse ((Int -> Gen tm) -> Int -> Maybe (Gen tm))
 
data TermParseRule m tm =
    TermParseBase (m tm)
  | TermParseRecurse (m tm -> m tm)

data TermPrettyRule tm =
    TermPrettyBase (tm -> Maybe Doc)
  | TermPrettyRecurse ((tm -> Doc) -> tm -> Maybe Doc)

data ValueRule tm =
    ValueBase (tm -> Maybe tm)
  | ValueRecurse ((tm -> Maybe tm) -> tm -> Maybe tm)

data SmallStepRule tm =
    SmallStepBase (tm -> Maybe tm)
  | SmallStepRecurse ((tm -> Maybe tm) -> tm -> Maybe tm)

data BigStepRule tm =
    BigStepBase (tm -> Maybe tm)
  | BigStepRecurse ((tm -> Maybe tm) -> tm -> Maybe tm)

data InferRule m ty tm =
    InferBase (tm -> Maybe (m ty))
  | InferRecurse ((tm -> m ty) -> tm -> Maybe (m ty))

data TermRules mP mI ty tm = TermRules {
    tmGenRules       :: [TermGenRule tm]
  , tmParseRules     :: [TermParseRule mP tm]
  , tmPrettyRules    :: [TermPrettyRule tm]
  , tmValueRules     :: [ValueRule tm]
  , tmSmallStepRules :: [SmallStepRule tm]
  , tmBigStepRules   :: [BigStepRule tm]
  , tmInferRules     :: [InferRule mI ty tm]
  }

instance Monoid (TermRules mP mI ty tm) where
  mempty =
    TermRules
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
  mappend tm1 tm2 =
    TermRules
      (mappend (tmGenRules tm1) (tmGenRules tm2))
      (mappend (tmParseRules tm1) (tmParseRules tm2))
      (mappend (tmPrettyRules tm1) (tmPrettyRules tm2))
      (mappend (tmValueRules tm1) (tmValueRules tm2))
      (mappend (tmSmallStepRules tm1) (tmSmallStepRules tm2))
      (mappend (tmBigStepRules tm1) (tmBigStepRules tm2))
      (mappend (tmInferRules tm1) (tmInferRules tm2))

data LanguageRules mP mI te ty tm = LanguageRules {
    typeErrorRules :: TypeErrorRules ty te
  , typeRules      :: TypeRules mP ty
  , termRules      :: TermRules mP mI ty tm
  }

instance Monoid (LanguageRules mP mI te ty tm) where
  mempty =
    LanguageRules
      mempty
      mempty
      mempty
  mappend l1 l2 =
    LanguageRules
      (mappend (typeErrorRules l1) (typeErrorRules l2))
      (mappend (typeRules l1) (typeRules l2))
      (mappend (termRules l1) (termRules l2))
