{-# LANGUAGE GADTs #-}
module Rules where

import Control.Applicative
import Data.Foldable
import Data.List (partition)
import Data.Maybe (fromMaybe)

import Data.Profunctor 

import Test.QuickCheck

{-
 - Rules should have a Gen t that matches the rule
 - We could use a GADT to make the matching prism available
 - 
 - Would be nice to be able to combine Gen's in the same way that we
 - combine prisms - ie have an applicative / alternative for Matchers
 -
 - Want to check that 
 -   this gen matches with this matcher
 -   this gen matches none of the other rules
 - Possibly split into axiom / step
 -
 - the precondition being able to step / the term not being a value should
 - be required for Steps
 -
 - We want to check that the rule survives inside eval
 -
 - We want these for values as well, so that we can get the right gens and
 - printers
 - -}

-- for app
-- data R t a = R { s :: t -> t , a :: Rule t a }
-- for alt
-- data R t a = R { s :: t -> t , a :: [Rule t a] }
--
-- lift this one into Step rather than Axiom
-- step :: R (t -> t)

data R t a b = R { s :: t -> Maybe t, rules :: a -> [Maybe b] }

data Matcher a where
  Matcher :: String -> Gen a -> ((a -> Maybe a) -> a -> Maybe t) -> (t -> a) -> Matcher a

matcherStep :: Matcher a -> (a -> Maybe a) -> a -> Maybe a
matcherStep (Matcher _ _ f g) step a = fmap g (f step a)

matcherEval :: [Matcher a] -> a -> Maybe a
matcherEval ms = 
  let
   step x = asum . map (\m -> matcherStep m step x) $ ms
  in
    step

data Rs a b = Rs (a -> [Maybe b])

instance Functor (Rs a) where
  fmap f (Rs g) = Rs (fmap (fmap (fmap f)) g)

instance Applicative (Rs a) where
  pure = Rs . pure . pure . pure
  Rs af <*> Rs ax = Rs $ liftA2 (liftA2 (<*>)) af ax

instance Alternative (Rs a) where
  empty = Rs $ const empty
  Rs af <|> Rs ax = Rs $ \a -> af a <|> ax a

-- would be nice to have a combinator to access the step function, which
-- pushes something from an axiom to a rule
data Rule t a = Axiom (t -> Maybe a) | Step ((t -> Maybe t) -> t -> Maybe a)

instance Functor (Rule t) where
  fmap f (Axiom g) = Axiom $ fmap (fmap f) g
  fmap f (Step g)  = Step $ fmap (fmap (fmap f)) g

instance Applicative (Rule t) where
  pure = Axiom . pure . pure
  -- do we want to use alternative here ?
  Axiom f <*> Axiom x = Axiom $ liftA2 (<*>) f x
  Axiom f <*> Step x = Step $ liftA2 (liftA2 (<*>)) (pure f) x
  Step f <*> Axiom x = Step $ liftA2 (liftA2 (<*>)) f (pure x)
  Step f <*> Step x  = Step $ liftA2 (liftA2 (<*>)) f x

data Rules t a = Rules [Rule t a]

makeEval :: Rules a a -> a -> Maybe a
makeEval (Rules rs) = 
  let
      isAxiom (Axiom _) = True
      isAxiom _ = False
      (as, ss) = partition isAxiom rs
      step x = asum (fmap (\(Axiom f) -> f x) as) <|> asum (fmap (\(Step f) -> f step x) ss) 
  in
    step

-- TODO fix point of step function, optionally with history

