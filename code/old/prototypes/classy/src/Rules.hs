{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Rules where

import Control.Lens

import Rules.Pattern
import Rules.GenShrink

liftL2 :: Prism' s1 a1 -> Prism' s2 a2 -> Prism' (s1, s2) (a1, a2)
liftL2 p1 p2 = prism
  (\(b1, b2) -> (review p1 b1, review p2 b2))
  (\s@(s1, s2) -> let
      x = (,) <$> preview p1 s1 <*> preview p2 s2
    in
      case x of
          Nothing -> Left s
          Just y -> Right y
   )

liftL3 :: Prism' s1 a1 -> Prism' s2 a2 -> Prism' s3 a3 -> Prism' (s1, s2, s3) (a1, a2, a3)
liftL3 p1 p2 p3 = prism
  (\(b1, b2, b3) -> (review p1 b1, review p2 b2, review p3 b3))
  (\s@(s1, s2, s3) -> let
      x = (,,) <$> preview p1 s1 <*> preview p2 s2 <*> preview p3 s3
    in
      case x of
          Nothing -> Left s
          Just y -> Right y
   )

data Rule t a =
    Value String (Pattern t a)
  | Base String (Pattern t a)
  | Inductive String (Pattern t a -> Pattern t a)

-- prop: everything should be covered by a rule
-- rules can be converted to patterns for testing

data Rules t a =
  Rules {
    values :: [Rule t a]
  , baseSteps :: [Rule t a]
  , inductiveSteps :: [Rule t a]
  }

instance Monoid (Rules t a) where
  mempty = Rules mempty mempty mempty
  mappend (Rules v1 b1 i1) (Rules v2 b2 i2) = Rules (mappend v1 v2) (mappend b1 b2) (mappend i1 i2)

value :: String -> Pattern t a -> Rules t a
value name pat = Rules [Value name pat] [] []

base :: String -> Pattern t a -> Rules t a
base name pat = Rules [] [Base name pat] []

inductive :: String -> (Pattern t a -> Pattern t a) -> Rules t a
inductive name step = Rules [] [] [Inductive name step]

data RuleResults t a =
  RuleResults {
    genValue :: GenShrink t a
  , genTerm :: GenShrink t a
  , step :: Pattern t a
  }

-- TODO fn to convert a RuleSet to RuleResults

-- TODO fn to convert from small step to big step
-- TODO specify big step semantics, test them in similar ways
  -- add some optimisations with Plated, show that that big step semantics are preserved

-- TODO tests
-- value don't step
-- non-values do step
-- whole term eval is the same for a rule eval with the gen for that rule
-- all of the others assume that we have type information
   -- mostly interested in progress
   -- need a typechecker to be able to work with preservation
   -- we could have (Type -> Gen a) versions of the genTerm / patternGen step functions
   -- related TODO would be good to pass genTerm through the patterns / rules, as the equivalent to the `id` prism

-- match TmAdd (TmInt i) (TmInt i)
-- match TmAdd step y
-- match TmAdd (TmInt i) step

-- we need a pattern for step
-- we need a pattern for id
-- - this doesn't need to be typesafe for now
-- - possibly want to have type information here, and the ability to pipe in something that works for that (or something that doesn't)
-- will need Gens for values to get overall gen happening
