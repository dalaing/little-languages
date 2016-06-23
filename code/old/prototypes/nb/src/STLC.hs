{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module STLC where

import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Either (isRight, isLeft, partitionEithers)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.String (IsString)
import Data.Function (on)
import Data.Foldable (asum)
import Control.Applicative
import Control.Lens
import Control.Monad (ap, unless)
import Test.QuickCheck

import Control.Monad.Reader
import Control.Monad.Except

import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import qualified Text.Parser.Char as PC
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Parser.Expression
import qualified Data.HashSet as HS
import qualified Text.Trifecta as T
import Text.Trifecta.Delta
import Text.Trifecta.Rendering

import Bound
import Bound.Name
import Bound.Scope
import Prelude.Extras

import qualified Data.Map as M

type Loc = T.Span

data Match s a = Match { match :: s -> Maybe a }

-- should we go to TypeF here?

{-
data TypeF l f =
    TyBool
  | TyInt
  | TyArr f f
  | TyLoc l f
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TypeF
-}

data Type = TyBool
          | TyInt
          | TyArr Type Type
          | TyLoc Loc Type
          deriving (Eq, Ord, Show)

makeClassyPrisms ''Type

-- matcher

mTBool :: Match Type ()
mTBool = Match $ preview _TyBool

mTInt :: Match Type ()
mTInt = Match $ preview _TyInt

mTArr :: Match Type (Type, Type)
mTArr = Match $ preview _TyArr

mTLoc :: Match Type (Loc, Type)
mTLoc = Match $ preview _TyLoc

-- app function, to apply a type to an arr and get a result or an error
-- especially if we want to work past the location annotation

-- precedence

-- printer

prTBool :: Type -> Maybe Doc
prTBool = fmap prTBool' . match mTBool
  where
    prTBool' = const $ PP.text "Bool"

prTInt :: Type -> Maybe Doc
prTInt = fmap prTInt' . match mTInt
  where
    prTInt' = const $ PP.text "Int"

-- TODO replace with a table
prTArr :: (Type -> Doc) -> Type -> Maybe Doc
prTArr pr = fmap prTArr' . match mTArr
  where
    prTArr' (t1, t2) = pr t1 <+> PP.text "->" <+> pr t2

prTLoc :: (Type -> Doc) -> Type -> Maybe Doc
prTLoc pr = fmap prTLoc' . match mTLoc
  where
    prTLoc' (_, t) = pr t

-- TODO combined printer

-- parser

-- need a style here, with the appropriate reserved words

pTBool :: (Monad m, TokenParsing m) => m Type
pTBool = undefined

pTInt :: (Monad m, TokenParsing m) => m Type
pTInt = undefined

pTArr :: (Monad m, TokenParsing m) => m (Type -> Type -> Type)
pTArr = undefined
-- pTArr = curry (review _TyArr) <$ tReserved "->"

-- TODO combined parser

-- gen

gTBool :: Gen Type
gTBool = pure (review _TyBool ())

gTInt :: Gen Type
gTInt = pure (review _TyInt ())

gTArr :: Gen Type -> Gen Type
gTArr g = curry (review _TyArr) <$> g <*> g

genType :: Gen Type
genType = sized genType'
  where
    genType' s =
      let
        zeros = [gTBool, gTInt]
        s' = s `div` 2
        child = genType' s'
        nonZeros = [gTArr child]
      in
        oneof $ zeros ++ if s == 0 then [] else nonZeros

-- shrink

sTBool :: Type -> Maybe [Type]
sTBool = fmap sTBool' . match mTBool
  where
    sTBool' = const []

sTInt :: Type -> Maybe [Type]
sTInt = fmap sTInt' . match mTInt
  where
    sTInt' = const []

sTArr :: (Type -> [Type]) -> Type -> Maybe [Type]
sTArr shr = fmap sTArr' . match mTArr
  where
    sTArr' (x, y) =
      shr x ++
      shr y ++
      fmap (`TyArr` y) (shr x) ++
      fmap (x `TyArr`) (shr y)

shrinkType :: Type -> [Type]
shrinkType t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    sTBool
  , sTInt
  , sTArr shrinkType
  ]

data Pattern n =
    VarP n
  | WildP
  | PLoc Loc (Pattern n)

-- utility

-- print
-- parse
-- gen
-- shrink

-- TODO could do with a tool for working with a list of patterns
-- like with let bindings for functions calls etc
-- reverse order binding, might make partial application straight forward

-- TODO, break up, use classy prisms to link back to main term
-- TODO use Pattern for Lam binding, add source location to the pattern
-- TODO abstract over naming type?
-- TODO if we distinguish between the use of Add and the primitive Add,
--      we can have the primitives added in a non-recursive manner

-- if we add hindly-milner constraint based inference,
-- we can have TmLam Binding ...
--- data Binding = Implicit | Explicit Type
-- at that point we probably want to add type annotations into the Term
data TermF n f a =
            TmVar a
          | TmApp (f a) (f a)
          | TmLam Type (Scope (Name n ()) f a)
          | TmBool Bool
          | TmIf (f a) (f a) (f a)
          | TmInt Int
          | TmAdd (f a) (f a)
          | TmMul (f a) (f a)
          | TmEq (f a) (f a)
          | TmLoc Loc (f a)
          deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

natTermF :: Functor f => (forall x. f x -> g x) -> TermF n f a -> TermF n g a
natTermF _ (TmVar x) = TmVar x
natTermF g (TmApp f x) = TmApp (g f) (g x)
natTermF g (TmLam t e) = TmLam t (hoistScope g e)
natTermF _ (TmBool b) = TmBool b
natTermF g (TmIf b t f) = TmIf (g b) (g t) (g f)
natTermF _ (TmInt i) = TmInt i
natTermF g (TmAdd x y) = TmAdd (g x) (g y)
natTermF g (TmMul x y) = TmMul (g x) (g y)
natTermF g (TmEq x y) = TmEq (g x) (g y)
natTermF g (TmLoc l x) = TmLoc l (g x)

lam' :: (Eq a, Monad f) => a -> f a -> Scope (Name a ()) f a
lam' v = abstract1Name v

lam :: (Eq a, Monad f) => Type -> a -> f a -> TermF a f a
lam ty v = TmLam ty . lam' v

-- data Term = Fix TermF Term
-- data LocTerm = Fix TermF (Maybe Loc, LocTerm)

makeClassyPrisms ''TermF

data Term n a = Term { unTerm :: TermF n (Term n) a }
            deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq n => Eq1 (Term n) where
  (==#) = (==)

instance Ord n => Ord1 (Term n) where
  compare1 = compare

instance Show n => Show1 (Term n) where
  showsPrec1 = showsPrec

instance Applicative (Term n) where
  pure = return
  (<*>) = ap

instance Monad (Term n) where
  return = Term . TmVar

  Term (TmVar x) >>= g = g x
  Term (TmApp f x) >>= g = Term (TmApp (f >>= g) (x >>= g))
  Term (TmLam t e) >>= g = Term (TmLam t (e >>>= g))
  Term (TmBool b) >>= _ = Term (TmBool b)
  Term (TmIf b t f) >>= g = Term (TmIf (b >>= g) (t >>= g) (f >>= g))
  Term (TmInt i) >>= _ = Term (TmInt i)
  Term (TmAdd x y) >>= g = Term (TmAdd (x >>= g) (y >>= g))
  Term (TmMul x y) >>= g = Term (TmMul (x >>= g) (y >>= g))
  Term (TmEq x y) >>= g = Term (TmEq (x >>= g) (y >>= g))
  Term (TmLoc l x) >>= g = Term (TmLoc l (x >>= g))

makeWrapped ''Term

instance AsTermF (Term n a) n (Term n) a where
  _TmVar = _Wrapped . _TmVar
  _TmApp = _Wrapped . _TmApp
  _TmLam = _Wrapped . _TmLam
  _TmBool = _Wrapped . _TmBool
  _TmIf = _Wrapped . _TmIf
  _TmInt = _Wrapped . _TmInt
  _TmAdd = _Wrapped . _TmAdd
  _TmMul = _Wrapped . _TmMul
  _TmEq = _Wrapped . _TmEq
  _TmLoc = _Wrapped . _TmLoc

data Ann n a = Ann { _term :: LocTerm n a, _ann :: Maybe Loc}
           deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative (Ann n) where
  pure = return
  (<*>) = ap

instance Monad (Ann n) where
  return x = Ann (return x) Nothing
  Ann t l >>= f = Ann (t >>= _term . f) l

instance Eq n => Eq1 (Ann n) where
  (==#) = (==)

instance Ord n => Ord1 (Ann n) where
  compare1 = compare

instance Show n => Show1 (Ann n) where
  showsPrec1 = showsPrec

data LocTerm n a = LocTerm { unLocTerm :: TermF n (Ann n) a}
            deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative (LocTerm n) where
  pure = return
  (<*>) = ap

locTermToAnn :: LocTerm n a -> Ann n a
locTermToAnn l = Ann l Nothing

instance Monad (LocTerm n) where
  return = LocTerm . TmVar

  LocTerm (TmVar x) >>= g = g x
  LocTerm (TmApp f x) >>= g = LocTerm (TmApp (f >>= locTermToAnn . g) (x >>= locTermToAnn . g))
  LocTerm (TmLam t e) >>= g = LocTerm (TmLam t (e >>>= locTermToAnn . g))
  LocTerm (TmBool b) >>= _ = LocTerm (TmBool b)
  LocTerm (TmIf b t f) >>= g = LocTerm (TmIf (b >>= locTermToAnn . g) (t >>= locTermToAnn . g) (f >>= locTermToAnn . g))
  LocTerm (TmInt i) >>= _ = LocTerm (TmInt i)
  LocTerm (TmAdd x y) >>= g = LocTerm (TmAdd (x >>= locTermToAnn . g) (y >>= locTermToAnn . g))
  LocTerm (TmMul x y) >>= g = LocTerm (TmMul (x >>= locTermToAnn . g) (y >>= locTermToAnn . g))
  LocTerm (TmEq x y) >>= g = LocTerm (TmEq (x >>= locTermToAnn . g) (y >>= locTermToAnn . g))
  LocTerm (TmLoc l x) >>= g = LocTerm (TmLoc l (x >>= locTermToAnn . g))

makeLenses ''Ann

makeWrapped ''LocTerm

instance AsTermF (LocTerm n a) n (Ann n) a where
  _TmVar = _Wrapped . _TmVar
  _TmApp = _Wrapped . _TmApp
  _TmLam = _Wrapped . _TmLam
  _TmBool = _Wrapped . _TmBool
  _TmIf = _Wrapped . _TmIf
  _TmInt = _Wrapped . _TmInt
  _TmAdd = _Wrapped . _TmAdd
  _TmMul = _Wrapped . _TmMul
  _TmEq = _Wrapped . _TmEq
  _TmLoc = _Wrapped . _TmLoc

-- If we're working with AsTermF f g
-- we need a helper to get us from g to the next f
--   for AsTermF Term Term, that is id
--   for AsTermF LocTerm Ann, that is term
-- this moves things from prisms to getters

stripLoc :: Term n a -> Term n a
stripLoc = Term . stripLoc' . unTerm

stripLoc' :: TermF n (Term n) a -> TermF n (Term n) a
stripLoc' (TmLoc _ t) = unTerm (stripLoc t) 
stripLoc' t = natTermF stripLoc t

termToAnn :: Term n a -> Ann n a
termToAnn (Term (TmLoc l t)) = Ann (termToLocTerm t) (Just l)
termToAnn (Term t) = Ann (termToLocTerm (Term t)) Nothing

termToLocTerm :: Term n a -> LocTerm n a
termToLocTerm = LocTerm . natTermF termToAnn . unTerm

annToTerm :: Ann n a -> Term n a
annToTerm (Ann t (Just l)) = Term (TmLoc l (locTermToTerm t))
annToTerm (Ann t Nothing) = locTermToTerm t

locTermToTerm :: LocTerm n a -> Term n a
locTermToTerm = Term . natTermF annToTerm . unLocTerm

-- pattern matching

mBool :: AsTermF (f a) n g a => Match (f a) Bool
mBool = Match $ preview _TmBool

mTrue :: AsTermF (f a) n g a => Match (f a) ()
mTrue = Match $ \t -> match mBool t >>= \b -> if b then Just () else Nothing

mFalse :: AsTermF (f a) n g a =>  Match (f a) ()
mFalse = Match $ \t -> match mBool t >>= \b -> if b then Nothing else Just ()

-- TODO possibly return these in eot form
mIf :: AsTermF (f a) n g a => Match (f a) (g a, g a, g a)
mIf = Match $ preview _TmIf

mInt :: AsTermF (f a) n g a => Match (f a) Int
mInt = Match $ preview _TmInt

mAdd :: AsTermF (f a) n g a => Match (f a) (g a, g a)
mAdd = Match $ preview _TmAdd

mMul :: AsTermF (f a) n g a => Match (f a) (g a, g a)
mMul = Match $ preview _TmMul

mEq :: AsTermF (f a) n g a => Match (f a) (g a, g a)
mEq = Match $ preview _TmEq

mLoc :: AsTermF (f a) n g a => Match (f a) (Loc, g a)
mLoc = Match $ preview _TmLoc

mVar :: AsTermF (f a) n g a => Match (f a) a
mVar = Match $ preview _TmVar

mApp :: AsTermF (f a) n g a => Match (f a) (g a, g a)
mApp = Match $ preview _TmApp

mLam :: AsTermF (f a) n g a => Match (f a) (Type, Scope (Name n ()) g a)
mLam = Match $ preview _TmLam

-- eventually we want to use this in the gen / shrink / infer code

{-
tTrue :: Type
tTrue = TyBool


tFalse = TyBool

tIf :: Type -> Type
tIf t = TyArr TyBool (TyArr t (TyArr t t))

tZero :: Type
tZero = TyNat

tSucc :: Type
tSucc = TyArr TyNat TyNat

tPred :: Type
tPred = TyArr TyNat TyNat

tIsZero :: Type
tIsZero = TyArr TyNat TyBool
-}

-- type checking and inference

data TypeInfo = TypeInfo Type (Maybe Loc)
               deriving (Eq, Ord, Show)

-- TODO
-- we can convert these to a good Doc?
-- and / or use Text.Trifecta.Result (Err)

data TypeError a = UnexpectedType { expected :: Type, actual :: TypeInfo}
               | Mismatch {type1 :: TypeInfo, type2 :: TypeInfo}
               | UnknownVariable a
               | UnknownType
               deriving (Eq, Ord, Show)

data TypeContext a = TypeContext { ctx :: M.Map a Type }

emptyContext :: TypeContext a
emptyContext = TypeContext M.empty

tyLookup :: Ord a => a -> TypeContext a -> Maybe Type
tyLookup v (TypeContext m) = M.lookup v m

tyExtend :: Ord a => a -> Type -> TypeContext a -> TypeContext a
tyExtend v t (TypeContext m) = TypeContext (M.insert v t m)

expect :: MonadError (TypeError a) m => Type -> TypeInfo -> m ()
expect ex t@(TypeInfo act _) =
  unless (ex == act) $
    throwError $ UnexpectedType ex t

expectEq :: MonadError (TypeError a) m => TypeInfo -> TypeInfo -> m ()
expectEq t1@(TypeInfo u1 _) t2@(TypeInfo u2 _) =
  unless (u1 == u2) $
    throwError $ Mismatch t1 t2

expectArr :: MonadError (TypeError a) m => TypeInfo -> m (TypeInfo, TypeInfo)
expectArr (TypeInfo t l) = case t of
  TyArr a b -> return (TypeInfo a l, TypeInfo b l)
  t -> throwError $ undefined

-- this is where we would use the source locations in patterns
inferVar :: (Ord a, MonadReader (TypeContext a) m, MonadError (TypeError a) m) => Ann n a -> Maybe (m TypeInfo)
inferVar (Ann t l) = fmap (inferVar' l) . match mVar $ t
  where
    inferVar' l v = do
      t <- asks (tyLookup v)
      case t of
        Nothing -> throwError $ UnknownVariable v
        Just u -> return $ TypeInfo u l

inferApp :: (Ord a, MonadReader (TypeContext a) m, MonadError (TypeError a) m) => (Ann n a -> m TypeInfo) -> Ann n a -> Maybe (m TypeInfo)
inferApp inf (Ann t l) = fmap (inferApp' inf l) . match mApp $ t
  where
    inferApp' inf l (f, x) = do
      tyF <- inf f
      (a, b) <- expectArr tyF
      tyX <- inf x
      expectEq a tyX
      return b

inferLam :: (MonadReader (TypeContext a) m, MonadError (TypeError a) m) => (Ann n a -> m TypeInfo) -> Ann n a -> Maybe (m TypeInfo)
inferLam inf (Ann t l) = fmap (inferLam' inf l) . match mLam $ t
  where
    inferLam' inf l (t, e) = do
      let r = undefined -- infer e with t in context
      return $ TypeInfo (TyArr t r) l

inferBool :: MonadError (TypeError a) m => Ann n a -> Maybe (m TypeInfo)
inferBool (Ann t l) = fmap (inferBool' l) . match mBool $ t
  where
    inferBool' = const . return . TypeInfo TyBool

-- converting the terms to types as they pass through would be useful
-- possibly would need TermF Type to get that done
inferIf :: MonadError (TypeError a) m => (Ann n a -> m TypeInfo) -> Ann n a -> Maybe (m TypeInfo)
inferIf inf (Ann t l) = fmap (inferIf' inf l) . match mIf $ t
  where
    inferIf' inf l (tm1, tm2, tm3) = do
      ty1 <- inf tm1
      expect TyBool ty1
      ty2@(TypeInfo t _) <- inf tm2
      ty3 <- inf tm3
      expectEq ty2 ty3
      return $ TypeInfo t l

inferInt :: MonadError (TypeError a) m => Ann n a -> Maybe (m TypeInfo)
inferInt (Ann t l) = fmap (inferInt' l) . match mInt $ t
  where
    inferInt' = const . return . TypeInfo TyInt

inferAdd :: MonadError (TypeError a) m => (Ann n a -> m TypeInfo) -> Ann n a -> Maybe (m TypeInfo)
inferAdd inf (Ann t l) = fmap (inferAdd' inf l) . match mAdd $ t
  where
    inferAdd' inf l (tm1, tm2) = do
      ty1 <- inf tm1
      expect TyInt ty1
      ty2 <- inf tm2
      expect TyInt ty2
      return $ TypeInfo TyInt l

inferMul :: MonadError (TypeError a) m => (Ann n a -> m TypeInfo) -> Ann n a -> Maybe (m TypeInfo)
inferMul inf (Ann t l) = fmap (inferMul' inf l) . match mMul $ t
  where
    inferMul' inf l (tm1, tm2) = do
      ty1 <- inf tm1
      expect TyInt ty1
      ty2 <- inf tm2
      expect TyInt ty2
      return $ TypeInfo TyInt l

inferEq :: MonadError (TypeError a) m => (Ann n a -> m TypeInfo) -> Ann n a -> Maybe (m TypeInfo)
inferEq inf (Ann t l) = fmap (inferEq' inf l) . match mEq $ t
  where
    inferEq' inf l (tm1, tm2) = do
      ty1 <- inf tm1
      expect TyInt ty1
      ty2 <- inf tm2
      expect TyInt ty2
      return $ TypeInfo TyBool l

inferLoc :: MonadError (TypeError a) m => (Ann n a -> m TypeInfo) -> Ann n a -> Maybe (m TypeInfo)
inferLoc inf (Ann t l) = fmap (inferLoc' inf l) . match mLoc $ t
  where
    inferLoc' inf _ (_, t) = inf t

-- we probably want to log warnings (for shadowing, at least) as we
-- run through the inference
-- we could log some of the errors as well, where the errors don't stop
-- inference from proceeding, ie (1 + false) could still have type Int
infer :: (Ord a, MonadReader (TypeContext a) m, MonadError (TypeError a) m) => Ann n a -> m TypeInfo
infer a =
  fromMaybe (throwError UnknownType) .
  asum .
  map ($ a) $
    [ inferVar
    , inferApp infer
    , inferLam infer
    , inferBool
    , inferIf infer
    , inferInt
    , inferAdd infer
    , inferMul infer
    , inferEq infer
    , inferLoc infer
    ]

-- values

bv :: Term n a -> Maybe (Term n a)
bv t = t <$ preview _TmBool t

nv :: Term n a -> Maybe (Term n a)
nv t = t <$ preview _TmInt t

-- no vars here, a free var should result in a stuck evaluator
lv :: Term n a -> Maybe (Term n a)
lv t@(Term (TmLam _ _)) = Just t
lv _ = Nothing

v :: Term n a -> Maybe (Term n a)
v t = asum [bv t, nv t, lv t]

-- small step semantics

sStepAppLam :: Term n a -> Maybe (Term n a)
sStepAppLam t = match mApp t >>= \(f, x) -> match mLam f >>= \(_, e) -> v x >>= \_ -> return $ instantiate1Name x e

sStepApp1 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepApp1 step t = match mApp t >>= \(f, x) -> step f >>= \f' -> return (review _TmApp (f', x))

-- this makes the language strict
sStepApp2 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepApp2 step t = match mApp t >>= \(f, x) -> v f >> step x >>= \x' -> return (review _TmApp (f, x'))

sStepIfTrue :: Term n a -> Maybe (Term n a)
sStepIfTrue t = match mIf t >>= \(b, c1, _) -> match mTrue b >> return c1

sStepIfFalse :: Term n a -> Maybe (Term n a)
sStepIfFalse t = match mIf t >>= \(b, _, c2) -> match mFalse b >> return c2

sStepIf :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepIf step t = match mIf t >>= \(b, c1, c2) -> step b >>= \b' -> return (review _TmIf (b', c1, c2))

sStepAddIntInt :: Term n a -> Maybe (Term n a)
sStepAddIntInt t = match mAdd t >>= \(t1, t2) -> match mInt t1 >>= \i1 -> match mInt t2 >>= \i2 -> return (review _TmInt (i1 + i2))

sStepAdd1 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepAdd1 step t = match mAdd t >>= \(t1, t2) -> step t1 >>= \u1 -> return (review _TmAdd (u1, t2))

sStepAdd2 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepAdd2 step t = match mAdd t >>= \(t1, t2) -> match mInt t1 >>= \_ -> step t2 >>= \u2 -> return (review _TmAdd (t1, u2))

sStepMulIntInt :: Term n a -> Maybe (Term n a)
sStepMulIntInt t = match mMul t >>= \(t1, t2) -> match mInt t1 >>= \i1 -> match mInt t2 >>= \i2 -> return (review _TmInt (i1 * i2))

sStepMul1 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepMul1 step t = match mMul t >>= \(t1, t2) -> step t1 >>= \u1 -> return (review _TmMul (u1, t2))

sStepMul2 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepMul2 step t = match mMul t >>= \(t1, t2) -> match mInt t1 >>= \_ -> step t2 >>= \u2 -> return (review _TmMul (t1, u2))

sStepEqIntInt :: Term n a -> Maybe (Term n a)
sStepEqIntInt t = match mEq t >>= \(t1, t2) -> match mInt t1 >>= \i1 -> match mInt t2 >>= \i2 -> return (review _TmBool (i1 == i2))

sStepEq1 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepEq1 step t = match mEq t >>= \(t1, t2) -> step t1 >>= \u1 -> return (review _TmEq (u1, t2))

sStepEq2 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepEq2 step t = match mEq t >>= \(t1, t2) -> match mInt t1 >>= \_ -> step t2 >>= \u2 -> return (review _TmEq (t1, u2))

sStepLoc :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
sStepLoc step t = match mLoc t >>= \(_, u) -> step u

sStep :: Term n a -> Maybe (Term n a)
sStep t = asum . map ($ t) $ [
    sStepAppLam
  , sStepApp1 sStep
  , sStepApp2 sStep
  , sStepIfTrue
  , sStepIfFalse
  , sStepIf sStep
  , sStepAddIntInt
  , sStepAdd1 sStep
  , sStepAdd2 sStep
  , sStepMulIntInt
  , sStepMul1 sStep
  , sStepMul2 sStep
  , sStepEqIntInt
  , sStepEq1 sStep
  , sStepEq2 sStep
  , sStepLoc sStep
  ]

-- need to repeat until just before it goes to nothing
sEval :: Term n a -> Term n a
sEval t = case sStep t of
  Nothing -> t
  Just u -> sEval u

-- big step semantics

-- https://www.cs.ubc.ca/~rxg/cpsc509/04-big-step.pdf
-- https://fos2015.github.io/project1.html

bStepApp :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
bStepApp step t = do
  (f, x) <- match mApp t
  f' <- step f
  (_, e) <- match mLam f'
  x' <- step x
  v x'
  v <- step (instantiate1Name x' e)
  return v

bStepIfTrue :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
bStepIfTrue step t = match mIf t >>= \(b, c1, _) -> step b >>= match mTrue >>= \_ -> step c1

bStepIfFalse :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
bStepIfFalse step t = match mIf t >>= \(b, _, c2) -> step b >>= match mFalse >>= \_ -> step c2

bStepAdd :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
bStepAdd step t = match mAdd t >>= \(t1, t2) -> step t1 >>= match mInt >>= \i1 -> step t2 >>= match mInt >>= \i2 -> return (review _TmInt (i1 + i2))

bStepMul :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
bStepMul step t = match mMul t >>= \(t1, t2) -> step t1 >>= match mInt >>= \i1 -> step t2 >>= match mInt >>= \i2 -> return (review _TmInt (i1 * i2))

bStepEq :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
bStepEq step t = match mEq t >>= \(t1, t2) -> step t1 >>= match mInt >>= \i1 -> step t2 >>= match mInt >>= \i2 -> return (review _TmBool (i1 == i2))

bStepLoc :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
bStepLoc step t = match mLoc t >>= \(_, u) -> step u

bStep :: Term n a -> Maybe (Term n a)
bStep t = asum . map ($ t) $ [
      v
    , bStepApp bStep
    , bStepIfTrue bStep
    , bStepIfFalse bStep
    , bStepAdd bStep
    , bStepMul bStep
    , bStepEq bStep
    , bStepLoc bStep
  ]

bEval :: Term n a -> Term n a
bEval t = fromMaybe t . bStep $ t

-- precedence

-- TODO, break up into pieces

data OperatorInfo = OperatorInfo { precedence :: Int, assoc :: Assoc }
                    deriving (Eq, Ord, Show)

precVar :: Maybe OperatorInfo
precVar = Nothing

precApp :: Maybe OperatorInfo
precApp = Just $ OperatorInfo 9 AssocLeft

precLam :: Maybe OperatorInfo
precLam = Nothing

precBool :: Maybe OperatorInfo
precBool = Nothing

precIf :: Maybe OperatorInfo
precIf = Nothing

precInt :: Maybe OperatorInfo
precInt = Nothing

precAdd :: Maybe OperatorInfo
precAdd = Just $ OperatorInfo 6 AssocLeft

precMul :: Maybe OperatorInfo
precMul = Just $ OperatorInfo 7 AssocLeft

precEq :: Maybe OperatorInfo
precEq = Just $ OperatorInfo 4 AssocNone

precLoc :: Maybe OperatorInfo
precLoc = Nothing

precTerm :: Term n a -> Maybe OperatorInfo
precTerm t = asum [ match mAdd t >> precAdd , match mMul t >> precMul, match mEq t >> precEq, match mApp t >> precApp]

-- printer

prVar :: Term n a -> Maybe Doc
prVar = fmap prVar' . match mVar
  where
    prVar' v = undefined

-- table driven printer will make nicer output here
prApp :: (Term n a -> Doc) -> Term n a -> Maybe Doc
prApp pr = fmap prApp' . match mApp
  where
    prApp' (x, y) = undefined

prLam :: (Term n a -> Doc) -> Term n a -> Maybe Doc
prLam pr = fmap prLam' . match mLam
  where
    prLam' (t, e) = undefined

prBool :: Term n a -> Maybe Doc
prBool = fmap prBool' . match mBool

prBool' :: Bool -> Doc
prBool' True = text "true"
prBool' False = text "false"

prIf :: (Term n a -> Doc) -> Term n a -> Maybe Doc
prIf pr = fmap (prIf' pr) . match mIf

prIf' :: (Term n a -> Doc) -> (Term n a, Term n a, Term n a) -> Doc
prIf' pr (b, c1, c2) = text "if" <+> pr b </> text "then" <+> pr c1 </> text "else" <+> pr c2

prInt :: Term n a -> Maybe Doc
prInt = fmap prInt' . match mInt

prInt' :: Int -> Doc
prInt' = int

{-
  (if 1 == 2 then 0 else 1) + 5
  if args have no or lower precedence, then add parens

  (3 + 4) * (5 + 6)
  TmMul (TmPlus 3 4) (TmPlus 5 6)

  3 + 4 * 5 + 6
  TmPlus (TmPlus 3 (TmMul 4 5)) 6

  What do we do about associativity?

  (3 / 4) / 5
  vs
  3 / (4 / 5)

  -- if args to if have a precedence, wrap them in params
  if true then 1 else (2 + 1)
  (if true then 1 else 2) + 1
  if an op has args with lower precedence, wrap them in brackets
  (3 + 4) * (5 + 6)


-}

prAdd :: (Term n a -> Doc) -> Term n a -> Maybe Doc
prAdd pr = fmap (prAdd' pr) . match mAdd

prAdd' :: (Term n a -> Doc) -> (Term n a, Term n a) -> Doc
prAdd' pr (t1, t2) = pr t1 <+> text "+" <+> pr t2

prMul :: (Term n a -> Doc) -> Term n a -> Maybe Doc
prMul pr = fmap (prMul' pr) . match mMul

prMul' :: (Term n a -> Doc) -> (Term n a, Term n a) -> Doc
prMul' pr (t1, t2) = pr t1 <+> text "*" <+> pr t2

prEq :: (Term n a -> Doc) -> Term n a -> Maybe Doc
prEq pr = fmap (prEq' pr) . match mEq

prEq' :: (Term n a -> Doc) -> (Term n a, Term n a) -> Doc
prEq' pr (t1, t2) = pr t1 <+> text "==" <+> pr t2

prLoc :: (Term n a -> Doc) -> Term n a -> Maybe Doc
prLoc pr = fmap (prLoc' pr) . match mLoc

prLoc' :: (Term n a -> Doc) -> (Loc, Term n a) -> Doc
prLoc' pr (_, t) = pr t

data PrinterType t =
    RegularPrinter (t -> Maybe Doc)
  | OpPrinter (t -> Maybe (t, t)) (Doc -> Doc -> Doc)

opApp :: PrinterType (Term n a)
opApp = OpPrinter (match mApp) (<+>)

opAdd :: PrinterType (Term n a)
opAdd = OpPrinter (match mAdd) (\x y -> x <+> text "+" <+> y)

opMul :: PrinterType (Term n a)
opMul = OpPrinter (match mMul) (\x y -> x <+> text "*" <+> y)

opEq :: PrinterType (Term n a)
opEq = OpPrinter (match mEq) (\x y -> x <+> text "==" <+> y)

printingTable :: [(Maybe OperatorInfo, PrinterType (Term n a))]
printingTable = [
    (precBool, RegularPrinter prBool)
  , (precIf, RegularPrinter (prIf prettyTerm))
  , (precInt, RegularPrinter prInt)
  , (precApp, opApp)
  , (precAdd, opAdd)
  , (precMul, opMul)
  , (precEq, opEq)
  ]

data PrOperator t = PrOperator (t -> Maybe (t, t)) (Doc -> Doc -> Doc) Assoc

groupPrOperator :: PrOperator t -> (t -> Maybe Doc) -> t -> Maybe Doc
groupPrOperator (PrOperator m pr _) fb =
  let
    r t = fb t <|> gr t
    gr t = do
      (x, y) <- m t
      dx <- r x
      dy <- r y
      return $ pr dx dy
  in
    r

-- order to build up try
-- for the first level of the table
   -- build a t -> Maybe Doc for the whole first level
      -- use asum [firstLevel ++ [fallBack]] to jump from args to docs
   -- second level uses second level + result of the first level for child nodes

-- if nothing else matches, use the fallback
buildTablePrinter :: [[PrOperator t]] -> (t -> Maybe Doc) -> t -> Maybe Doc
buildTablePrinter ops fb = undefined

-- currently only handles infix
-- prefix / infix / postfix probably belongs in OperatorInfo
printTerm :: Term n a -> Doc
printTerm = fromMaybe PP.empty . expr
  where
    table = fmap (fmap snd) . groupBy ((==) `on` fst) . sortBy (comparing fst) $ (ops >>= toOps)
    toOps (Just (OperatorInfo p a), m, pr) = [(p, (PrOperator m pr a))]
    toOps (Nothing, _, _) = []

    expr = buildTablePrinter table term
    term t = fmap PP.parens (expr t) <|> regular t
    regular t = asum . map ($ t) $ regulars
    (regulars, ops) = partitionEithers . map pt $ printingTable
    pt (_, RegularPrinter r) = Left r
    pt (i, OpPrinter m pr) = Right (i, m, pr)

prettyTerm :: Term n a -> Doc
prettyTerm t =
  fromMaybe PP.empty .
  asum .
  map ($ t) $ [
    prVar
  , prApp prettyTerm
  , prLam prettyTerm
  , prBool
  , prIf prettyTerm
  , prInt
  , prAdd prettyTerm
  , prMul prettyTerm
  , prEq prettyTerm
  , prLoc prettyTerm
  ]

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyString :: Term n a -> String
prettyString = docString . prettyTerm

-- parser

style :: PC.CharParsing m => IdentifierStyle m
style = IdentifierStyle "nb" PC.lower PC.alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["true", "false", "if", "then", "else", "+", "=="]


identifier :: (Monad m, TokenParsing m, IsString a) => m a
identifier = ident style

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

data ParserType m f = ParseRegular (m f) | ParseOp (m (f -> f -> f))

parseVar :: (Monad m, TokenParsing m, IsString a) => m (Term n a)
parseVar = fmap (review _TmVar) (identifier T.<?> "var")

parseApp :: (Monad m, TokenParsing m) => m (Term n a -> Term n a -> Term n a)
parseApp = curry (review _TmApp) <$ reserved " "

parseLam :: (Monad m, TokenParsing m) => m (Term n a) -> m (Term n a)
parseLam p = undefined

parseBool :: (Monad m, TokenParsing m) => m (Term n a)
parseBool = fmap (review _TmBool) (parseBool' T.<?> "bool")
  where
    parseBool' =
      True <$ reserved "true" <|>
      False <$ reserved "false"

parseIf :: (Monad m, TokenParsing m) => m (Term n a) -> m (Term n a)
parseIf p = fmap (review _TmIf) ((,,) <$ reserved "if" <*> p <* reserved "then" <*> p <* reserved "else" <*> p T.<?> "if-then-else")

parseInt :: (Monad m, TokenParsing m) => m (Term n a)
parseInt = fmap (review _TmInt . fromInteger) (T.integer T.<?> "int")

parseAdd :: (Monad m, TokenParsing m) => m (Term n a -> Term n a -> Term n a)
parseAdd = curry (review _TmAdd) <$ reserved "+"

parseMul :: (Monad m, TokenParsing m) => m (Term n a -> Term n a -> Term n a)
parseMul = curry (review _TmMul) <$ reserved "*"

parseEq :: (Monad m, TokenParsing m) => m (Term n a -> Term n a -> Term n a)
parseEq = curry (review _TmEq) <$ reserved "=="

parseLoc :: (Monad m, T.DeltaParsing m) => m (Term n a) -> m (Term n a)
parseLoc p = do
  (t :~ l) <- T.spanned p
  return $ review _TmLoc (l, t)

parsingTable :: (Monad m, T.DeltaParsing m, IsString a) => [(Maybe OperatorInfo, ParserType m (Term n a))]
parsingTable = [
    (precVar, ParseRegular parseVar)
  , (precLam, ParseRegular (parseLam parseTerm))
  , (precBool, ParseRegular parseBool)
  , (precIf, ParseRegular (parseIf parseTerm))
  , (precInt, ParseRegular parseInt)
  , (precLam, ParseOp parseApp)
  , (precAdd, ParseOp parseAdd)
  , (precMul, ParseOp parseMul)
  , (precEq, ParseOp parseEq)
  ]

-- can build the table by grabbing the precedence associated with
-- each parser

-- no precednce -> term parser
-- has precedence -> expression parser

parseTerm :: (Monad m, T.DeltaParsing m, IsString a) => m (Term n a)
parseTerm = expr
  where
    table = fmap (fmap snd) . groupBy ((==) `on` fst) . sortBy (comparing fst) $ (ops >>= toOps)
    toOps (Just (OperatorInfo p a), o) = [(p, Infix o a)]
    toOps (Nothing, _) = []
    expr = parseLoc (buildExpressionParser table term)
    term = T.parens expr <|> (parseLoc . asum . map snd $ regulars)
    (regulars, ops) = partitionEithers . map pt $ parsingTable
    pt (i, ParseRegular r) = Left (i, r)
    pt (i, ParseOp o) = Right (i, o)

parseString :: IsString a => String -> Either String (Term n a)
parseString s = case T.parseString parseTerm (Lines 0 0 0 0) s of
  T.Success r -> Right r
  T.Failure d -> Left (docString d)

-- gen and shrink

gVar :: Gen (Term n a)
gVar = review _TmVar <$> undefined

gVarContaining :: Term n a -> Maybe (Gen (Term n a))
gVarContaining = const Nothing

gApp' :: Gen (Term n a) -> Gen (Term n a) -> Gen (Term n a)
gApp' t1 t2 = curry (review _TmApp) <$> t1 <*> t2

gApp :: Gen (Term n a) -> Gen (Term n a)
gApp t = gApp' t t

-- gc t includes pure t
gAppContaining :: Gen (Term n a) -> (Term n a -> Gen (Term n a)) -> Term n a -> Gen (Term n a)
gAppContaining g gc t =
  oneof [
    gApp' (gc t) g
  , gApp' g (gc t)
  ]

-- TODO pass in the recursive calls that we need,
-- so that we can build them up elsewhere / keep them modular
gLam :: Eq a => Gen a -> Gen (Term a a) -> Gen (Term a a)
gLam gv t = do
  tyA <- genType
  v <- gv
  tyB <- genType
  e <- genTermWithTypeContaining tyB (review _TmVar v)
  return $ review _TmLam (tyA, lam' v e)

gBool :: Gen (Term n a)
gBool = fmap (review _TmBool) arbitrary

gBoolContaining :: Term n a -> Maybe (Gen (Term n a))
gBoolContaining = const Nothing

gIf' :: Gen (Term n a) -> Gen (Term n a) -> Gen (Term n a) -> Gen (Term n a)
gIf' t1 t2 t3 = fmap (review _TmIf) ((,,) <$> t1 <*> t2 <*> t3)

gIf :: Gen (Term n a) -> Gen (Term n a) -> Gen (Term n a)
gIf t1 t2 = gIf' t1 t2 t2

gIfContaining :: Gen (Term n a)
              -> (Term n a -> Gen (Term n a))
              -> Gen (Term n a)
              -> (Term n a -> Gen (Term n a))
              -> Term n a
              -> Gen (Term n a)
gIfContaining g1 gc1 g2 gc2 t =
  oneof [
    gIf' (gc1 t) g2 g2
  , gIf' g1 (gc2 t) g2
  , gIf' g1 g2 (gc2 t)
  ]

gInt :: Gen (Term n a)
gInt = fmap (review _TmInt) arbitrary

gIntContaining :: Term n a -> Maybe (Gen (Term n a))
gIntContaining = const Nothing

gAdd' :: Gen (Term n a) -> Gen (Term n a) -> Gen (Term n a)
gAdd' t1 t2 = curry (review _TmAdd) <$> t1 <*> t2

gAdd :: Gen (Term n a) -> Gen (Term n a)
gAdd t = gAdd' t t

gAddContaining :: Gen (Term n a) -> (Term n a -> Gen (Term n a)) -> Term n a -> Gen (Term n a)
gAddContaining g gc t =
  oneof [
    gAdd' (gc t) g
  , gAdd' g (gc t)
  ]

gMul' :: Gen (Term n a) -> Gen (Term n a) -> Gen (Term n a)
gMul' t1 t2 = curry (review _TmMul) <$> t1 <*> t2

gMul :: Gen (Term n a) -> Gen (Term n a)
gMul t = gMul' t t

gMulContaining :: Gen (Term n a) -> (Term n a -> Gen (Term n a)) -> Term n a -> Gen (Term n a)
gMulContaining g gc t =
  oneof [
    gMul' (gc t) g
  , gMul' g (gc t)
  ]

gEq' :: Gen (Term n a) -> Gen (Term n a) -> Gen (Term n a)
gEq' t1 t2 = curry (review _TmEq) <$> t1 <*> t2

gEq :: Gen (Term n a) -> Gen (Term n a)
gEq t = gEq' t t

gEqContaining :: Gen (Term n a) -> (Term n a -> Gen (Term n a)) -> Term n a -> Gen (Term n a)
gEqContaining g gc t =
  oneof [
    gEq' (gc t) g
  , gEq' g (gc t)
  ]

genVar :: Gen a
genVar = undefined

genAny :: Eq a => Gen (Term a a)
genAny = sized genAny'

genAny' :: Eq a => Int -> Gen (Term a a)
genAny' s =
    oneof $ zeroSize ++ if s == 0 then [] else otherSize
  where
    zeroSize = [gVar, gBool, gInt]
    child = genAny' (s `div` 2)
    otherSize = [gApp child, gLam genVar child, gIf child child, gAdd child, gMul child, gEq child]

genAnyContaining :: Term n a -> Gen (Term n a)
genAnyContaining t = undefined

genTerm :: Gen (Term n a)
genTerm = sized $ genTerm' Nothing

genTermWithType :: Type -> Gen (Term n a)
genTermWithType t = sized $ genTerm' (Just t)

genTerm' :: Maybe Type -> Int -> Gen (Term n a)
genTerm' Nothing s =
  oneof [
    genTerm' (Just TyBool) s
  , genTerm' (Just TyInt) s
  ]
genTerm' (Just TyInt) s =
  let
    zeroSize = [gInt]
    s' = s `div` 2
    gB = genTerm' (Just TyBool) s'
    gI = genTerm' (Just TyInt) s'
    otherSize = [gIf gB gI, gAdd gI, gMul gI]
  in
    oneof $ zeroSize ++ if s == 0 then [] else otherSize
genTerm' (Just TyBool) s =
  let
    zeroSize = [gBool]
    s' = s `div` 2
    gB = genTerm' (Just TyBool) s'
    gI = genTerm' (Just TyInt) s'
    otherSize = [gEq gI, gIf gB gB]
  in
    oneof $ zeroSize ++ if s == 0 then [] else otherSize

genTermContaining :: Term n a -> Gen (Term n a)
genTermContaining t = sized $ genTermContaining' Nothing t

genTermWithTypeContaining :: Type -> Term n a -> Gen (Term n a)
genTermWithTypeContaining ty tm = sized $ genTermContaining' (Just ty) tm

genTermContaining' :: Maybe Type -> Term n a -> Int -> Gen (Term n a)
genTermContaining' = undefined

-- keep things well typed in shrinking
-- - we have gens for dealing with ill typed stuff

sVar :: Term n a -> Maybe [Term n a]
sVar = fmap sVar' . match mVar
  where
    sVar' = const []

sApp :: (Term n a -> [Term n a]) -> Term n a -> Maybe [Term n a]
sApp shr = fmap sApp' . match mApp
  where
    sApp' (x, y) =
      fmap (\x' -> review _TmApp (x', y)) (shr x) ++
      fmap (\y' -> review _TmApp (x, y')) (shr y)

sLam :: (forall x. Term n x -> [Term n x]) -> Term n a -> Maybe [Term n a]
sLam shr = fmap sLam' . match mLam
  where
    sLam' (t, e) =
      let
        es = fmap toScope . shr . fromScope $ e
      in
        fmap (\x -> review _TmLam (t, x)) es

sBool :: Term n a -> Maybe [Term n a]
sBool = fmap sBool' . match mBool
  where
    sBool' = const []

sIf :: (Term n a -> [Term n a]) -> Term n a -> Maybe [Term n a]
sIf shr = fmap (sIf' shr) . match mIf

sIf' :: (Term n a -> [Term n a]) -> (Term n a, Term n a, Term n a) -> [Term n a]
sIf' shr (t1, t2, t3) =
  shr t2 ++
  shr t3 ++
  fmap (\t -> review _TmIf (t, t2, t3)) (shr t1) ++
  fmap (\t -> review _TmIf (t1, t, t3)) (shr t2) ++
  fmap (\t -> review _TmIf (t1, t2, t)) (shr t3)

sInt :: Term n a -> Maybe [Term n a]
sInt = fmap sInt' . match mInt

sInt' :: Int -> [Term n a]
sInt' = const []

sAdd :: (Term n a -> [Term n a]) -> Term n a -> Maybe [Term n a]
sAdd shr = fmap (sAdd' shr) . match mAdd

sAdd' :: (Term n a -> [Term n a]) -> (Term n a, Term n a) -> [Term n a]
sAdd' shr (t1, t2) =
  shr t1 ++
  shr t2 ++
  fmap (\t -> review _TmAdd (t, t2)) (shr t1) ++
  fmap (\t -> review _TmAdd (t1, t)) (shr t2)

sMul :: (Term n a -> [Term n a]) -> Term n a -> Maybe [Term n a]
sMul shr = fmap (sMul' shr) . match mMul

sMul' :: (Term n a -> [Term n a]) -> (Term n a, Term n a) -> [Term n a]
sMul' shr (t1, t2) =
  shr t1 ++
  shr t2 ++
  fmap (\t -> review _TmMul (t, t2)) (shr t1) ++
  fmap (\t -> review _TmMul (t1, t)) (shr t2)

sEq :: (Term n a -> [Term n a]) -> Term n a -> Maybe [Term n a]
sEq shr = fmap (sEq' shr) . match mEq

sEq' :: (Term n a -> [Term n a]) -> (Term n a, Term n a) -> [Term n a]
sEq' shr (t1, t2) = 
  fmap (\t -> review _TmEq (t, t2)) (shr t1) ++
  fmap (\t -> review _TmEq (t1, t)) (shr t2)

sLoc :: (Term n a -> [Term n a]) -> Term n a -> Maybe [Term n a]
sLoc shr = fmap (sLoc' shr) . match mLoc

sLoc' :: (Term n a -> [Term n a]) -> (Loc, Term n a) -> [Term n a]
sLoc' shr (s, t) = fmap (\t -> review _TmLoc (s,t)) (shr t)

shrinkTerm :: Term n a -> [Term n a]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    sVar
  , sApp shrinkTerm
  , sLam shrinkTerm
  , sBool
  , sIf shrinkTerm
  , sInt
  , sAdd shrinkTerm
  , sMul shrinkTerm
  , sEq shrinkTerm
  , sLoc shrinkTerm
  ]

instance Arbitrary (Term n a) where
  arbitrary = genTerm
  shrink = shrinkTerm

newtype IllTerm a = IllTerm { runIll :: Term a a }
                  deriving (Eq, Ord, Show)

instance Eq a => Arbitrary (IllTerm a) where
   arbitrary = IllTerm <$> genAny
   shrink = fmap IllTerm . shrinkTerm . runIll

newtype SrcTerm a = SrcTerm { runSrc :: Term a a}
                  deriving (Eq, Ord, Show)

instance IsString a => Arbitrary (SrcTerm a) where
  arbitrary = (SrcTerm . fromRight . parseString . prettyString) <$> genTerm
    where
      fromRight (Right x) = x
  shrink = fmap SrcTerm . shrinkTerm . runSrc

newtype SrcIllTerm a = SrcIllTerm { runSrcIll :: Term a a}
                  deriving (Eq, Ord, Show)

instance (Eq a, IsString a) => Arbitrary (SrcIllTerm a) where
  arbitrary = (SrcIllTerm . fromRight . parseString . prettyString) <$> (genAny :: Gen (Term a a))
    where
      fromRight (Right x) = x
  shrink = fmap SrcIllTerm . shrinkTerm . runSrcIll

propWellTyped :: Term String String -> Bool
propWellTyped =
  isRight .
  runExcept .
  flip runReaderT emptyContext .
  infer .
  termToAnn

propSmallBig :: Term String String -> Bool
propSmallBig = (==) <$> sEval <*> bEval

-- we should be able test this for each of the individual steps
-- concretely:
-- - the gen for each step should behave the same on
--     - the rule for the step
--     - the overal step function
propPreservation :: Term String String -> Bool
propPreservation t =
  case sStep t of
    Nothing -> True
    Just u -> (runExcept . flip runReaderT emptyContext . infer . termToAnn) t == (runExcept . flip runReaderT emptyContext . infer . termToAnn) u

propProgress :: Term String String -> Bool
propProgress t = case v t of
  Just _ -> True
  Nothing -> isJust (sStep t)

-- TODO progress and preservation for big step semantics

-- TODO test to show that constant folding doesn't effect the output
-- - via the overall evaluator
-- - via the individual rules? or is that overkill? we built the evaluator for model checking...
-- - might be worth showing that it reduces the number of small steps needed to be made?
-- - do we annotate the small steps with a cost bound, use that to demonstrate savings?
-- - or at least that the cost doesn't grow

propIll :: IllTerm String -> Bool
propIll (IllTerm t) =
  isJust (v t) ||
  isJust (sStep t) ||
  isLeft (runExcept . flip runReaderT emptyContext . infer . termToAnn $ t)

propRoundTrip :: Term String String -> Property
propRoundTrip t = propRoundTrip' t
  where
    propRoundTrip' = (===) <$> Right <*> (fmap stripLoc . parseString . prettyString)
