{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module Term where

import Control.Lens
import Control.Error.Util (hush)
import Data.Bifunctor
import Data.Bitraversable
import Data.Profunctor
import Data.Foldable
import Data.Maybe

import Generics.Eot
import Test.QuickCheck

class Contravariant1 p where
  contramap1 :: (b -> a) -> p a c -> p b c

class ProApply p where
  conquer1 :: (a -> ()) -> p a ()
  proap :: (a -> (b, c))
        -> ((d, e) -> f)
        -> p b d
        -> p c e
        -> p a f

class ProAlt p where
  lose1 :: (a -> Void) -> p a Void
  proalt :: (a -> Either b c)
         -> (Either d e -> f)
         -> p b d
         -> p c e
         -> p a f

-- want to pass in the fix point operation in here
class ProApply p => ProFix p where
  profix :: (a -> (a, c))
         -> ((d, e) -> d)
         -> p a d
         -> p c e
         -> p a d
  profix = proap

instance ProApply (->) where
  conquer1 _ = const ()
  proap scatter gather x y a =
    case scatter a of
      (b, c) -> curry gather (x b) (y c)

instance ProAlt (->) where
  lose1 f = f
  proalt scatter gather x y a =
    case scatter a of
      Left b -> gather . Left . x $ b
      Right c -> gather . Right . y $ c

instance Applicative f => ProApply (Star f) where
  conquer1 _ = Star . const . pure $ ()
  proap scatter gather (Star x) (Star y) =
    Star $ fmap gather . bitraverse x y . scatter

instance Applicative f => ProAlt (Star f) where
  lose1 _ = Star . const . pure $ error "boom"
  proalt scatter gather (Star x) (Star y) =
    Star $ fmap gather . bitraverse x y . scatter

explode :: (HasEot t, Profunctor p) => p (Eot t) (Eot t) -> p t t
explode = dimap toEot fromEot

infixr 4 <<*>>
(<<*>>) :: ProApply p => p b d -> p c e -> p (b,c) (d,e)
(<<*>>) = proap id id

infixr 3 <<|>>
(<<|>>) :: ProAlt p => p b d -> p c e -> p (Either b c) (Either d e)
(<<|>>) = proalt id id

data Term =
    TmInt Int
  | TmAdd Term Term
  | TmMul Term Term
  deriving (Eq, Ord, Show, Generic)

makeClassyPrisms ''Term

genTmInt :: Gen Term
genTmInt = TmInt <$> arbitrary

shrinkTmInt :: Term -> Maybe [Term]
shrinkTmInt (TmInt i) = Just . fmap TmInt . shrink $ i
shrinkTmInt _ = Nothing

genTmAdd :: Gen Term -> Gen Term
genTmAdd t = TmAdd <$> t' <*> t'
  where
    t' = sized (\s -> resize (s `div` 2) t)

shrinkTmAdd :: (Term -> [Term]) -> Term -> Maybe [Term]
shrinkTmAdd shr (TmAdd x y) = Just $ x : y : shr x ++ fmap (TmAdd x) (shr y) ++ shr y ++ fmap (`TmAdd` y) (shr x)
shrinkTmAdd shr _ = Nothing

genTmMul :: Gen Term -> Gen Term
genTmMul t = TmMul <$> t' <*> t'
  where
    t' = sized (\s -> resize (s `div` 2) t)

shrinkTmMul :: (Term -> [Term]) -> Term -> Maybe [Term]
shrinkTmMul shr (TmMul x y) = Just $ x : y : shr x ++ fmap (TmMul x) (shr y) ++ shr y ++ fmap (`TmMul` y) (shr x)
shrinkTmMul shr _ = Nothing

genTerm :: Gen Term
genTerm = oneof [genTmInt, genTmAdd genTerm, genTmMul genTerm]

-- TODO need to work profix into there
genTerm2 :: WGen Term Term
genTerm2 = explode genTerm2'
  where
    genTerm2' =
      WGen arbitrary <<*>> conquer1 (const ()) <<|>>
      genTerm2 <<*>> genTerm2 <<*>> conquer1 (const ()) <<|>>
      genTerm2 <<*>> genTerm2 <<*>> conquer1 (const ()) <<|>>
      lose1 (const (error "boom"))

-- shrinkTerm :: Term -> [Term]
-- shrinkTerm = fromMaybe [] . asum [shrinkTmInt, shrinkTmAdd shrinkTerm, shrinkTmMul shrinkTerm]

data WGen b a = WGen (Gen a)

instance Profunctor WGen where
  dimap l r (WGen g) = WGen (fmap r g)

instance ProApply WGen where
  conquer1 f = WGen (pure ())
  proap _ gather (WGen g1) (WGen g2) = WGen $ fmap gather ((,) <$> g1 <*> g2)

instance ProAlt WGen where
  lose1 f = WGen (pure (error "WGen: lose1"))
  proalt _ gather (WGen g1) (WGen g2) = WGen $ oneof [fmap (gather . Left) g1, fmap (gather . Right) g2]

-- TODO we want to make this EoT compatible, in which case we should only half the first argument
-- it would also be nice to be able to gather the recursive arguments, so we could count them (n)
-- and resize them to 1/n th of their original sizes
instance ProFix WGen where
  profix _ gather (WGen g1) (WGen g2) = WGen $ fmap gather ((,) <$> half g1 <*> half g2)
    where
      half g = sized (\s -> resize (s `div` 2) g)

data Shrink s t = Shrink (s -> [t])

instance Profunctor Shrink where
  dimap l r (Shrink f) = Shrink (dimap l (fmap r) f)

instance Functor (Shrink s) where
  fmap = rmap

instance Contravariant1 Shrink where
  contramap1 = lmap

instance ProApply Shrink where
  conquer1 _ = Shrink (const [])
  proap scatter gather (Shrink s1) (Shrink s2) = Shrink $ undefined . scatter

instance ProFix Shrink where
  profix scatter gather (Shrink s1) (Shrink s2) = Shrink $ undefined . scatter

data GenShrink s t = GenShrink (WGen s t) (Shrink s t)

instance Profunctor GenShrink where
  dimap l r (GenShrink g s) = GenShrink (dimap l r g) (dimap l r s)

instance Functor (GenShrink s) where
  fmap = rmap

instance Contravariant1 GenShrink where
  contramap1 = lmap

instance ProApply GenShrink where
  conquer1 f = GenShrink (conquer1 f) (conquer1 f)
  proap scatter gather (GenShrink g1 s1) (GenShrink g2 s2) =
    GenShrink (proap scatter gather g1 g2) (proap scatter gather s1 s2)

instance ProFix GenShrink where
  profix scatter gather (GenShrink g1 s1) (GenShrink g2 s2) =
    GenShrink (profix scatter gather g1 g2) (profix scatter gather s1 s2)

data Pattern a s t = Pattern { scatter :: s -> Maybe a, gather :: a -> t}
-- convert from prism

instance Profunctor (Pattern a) where
  dimap l r (Pattern s g) = Pattern (lmap l s) (rmap r g)

-- pAdd :: Term -> Maybe (Term, Term)
-- pIntLit :: Term -> Maybe Int

-- if we can explode into an eot and litter maybes throughout
-- can we layer these patterns on top of each other, so that they don't have
-- to redo various pattern matches?
-- might need George's type with Conflict in it, for when we union two branches that collide
-- easy enough when we're following the shape of an ADT
-- otherwise we maybe need to collect pattern matches that lead to maybes, and check that only one of them succeeds
-- - this is usually only used for step / any
--   - if any is only used in the right most position, this is a non-issue
--     - otherwise we need to check the whole product for a match to see if there is an overlap

-- we'd take a list of scatter / gather pairs
-- we'd combine the patterns and fuse in the surviving gather to get the step function
-- we can pass the context in via the patterns, and tie the knot during the combination of patterns

-- if we can deal with (a -> (Int, b)) for things like frequency
-- - can we push that Int down into the children (with a similar form)
-- - the goal would be to combine all these things
--   then annotate the frequencies at the combined level
--   then make use of that inside the individual components (like Gen / frequency, or the order of pattern matching)

data Pattern2 s a = Pattern2 (s -> Maybe a)

instance Profunctor Pattern2 where
  dimap l r (Pattern2 f) = Pattern2 (dimap l (fmap r) f)

instance ProApply Pattern2 where
  conquer1 _ = Pattern2 (const Nothing)
  proap scatter gather (Pattern2 p1) (Pattern2 p2) = Pattern2 $ fmap gather . bitraverse p1 p2 . scatter

prismToPattern :: Prism' s a -> Pattern2 s a
prismToPattern p = withPrism p $ \_ t -> Pattern2 (hush . t)

pInt :: Pattern2 Term Int
pInt = prismToPattern _TmInt

pAdd :: Pattern2 Term (Term, Term)
pAdd = prismToPattern _TmAdd

o :: Pattern2 b c -> Pattern2 a b -> Pattern2 a c
o = undefined

pAddIntInt :: Pattern2 Term (Int, Int)
pAddIntInt = proap id id pInt pInt `o` pAdd

pId :: Pattern2 a a
pId = Pattern2 Just

-- this has gone past patterm matching and into evaluation
pAdd1 :: Pattern2 Term Term -> Pattern2 Term Term
pAdd1 p = proap id (uncurry TmAdd) p pId `o` pAdd

-- would be better with something that round tripped through the scatter and the gaterh for pInt
pAdd2 :: Pattern2 Term Term -> Pattern2 Term Term
pAdd2 p = proap id (uncurry TmAdd . first TmInt) pInt p `o` pAdd

data Step s t = Step (s -> Maybe t)

test :: Pattern a s s -> Step s s
test p = Step $ fmap (gather p) . scatter p

instance Profunctor Step where
  dimap l r (Step f) = Step (dimap l (fmap r) f)

instance Functor (Step s) where
  fmap = rmap

instance Contravariant1 Step where
  contramap1 = lmap

instance ProApply Step where
  conquer1 _ = Step (const Nothing)
  proap scatter gather (Step f) (Step g) = Step $ fmap gather . bitraverse f g . scatter

-- test p = fmap (gather p) . scatter p
-- proap (scatter pAdd) (gather pIntLit . uncurry (+)) (scatter pIntLit) (scatter pIntLit)
-- proap (scatter pAdd) (gather pAdd) step pure
-- proap (scatter pAdd) (gather pAdd) (test pIntLit) step

-- Hmmm.  test isn't a step, but it's being used like one
-- we should probably do a better job of composing the patterns in the first place
