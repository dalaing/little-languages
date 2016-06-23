{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module IB where

import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Either (isRight, isLeft, partitionEithers)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Foldable (asum)
import Control.Applicative
import Control.Lens
import Test.QuickCheck

import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import qualified Text.Parser.Char as PC
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Parser.Expression
import qualified Data.HashSet as HS
import qualified Text.Trifecta as T
import Text.Trifecta.Delta
import Text.Trifecta.Rendering

type Loc = T.Span

-- TODO, break up, use classy prisms to link back to main term
data TermF f =
            TmBool Bool
          | TmIf f f f
          | TmInt Int
          | TmAdd f f
          | TmMul f f
          | TmEq f f
          | TmLoc Loc f
          deriving (Eq, Ord, Show, Functor)

-- data Term = Fix TermF Term
-- data LocTerm = Fix TermF (Maybe Loc, LocTerm)

makeClassyPrisms ''TermF

data Term = Term { unTerm :: TermF Term }
            deriving (Eq, Ord, Show)

makeWrapped ''Term

instance AsTermF Term Term where
  _TmBool = _Wrapped . _TmBool
  _TmIf = _Wrapped . _TmIf
  _TmInt = _Wrapped . _TmInt
  _TmAdd = _Wrapped . _TmAdd
  _TmMul = _Wrapped . _TmMul
  _TmEq = _Wrapped . _TmEq
  _TmLoc = _Wrapped . _TmLoc

data Ann = Ann { _term :: LocTerm, _ann :: Maybe Loc}
           deriving (Eq, Ord, Show)

data LocTerm = LocTerm { unLocTerm :: TermF Ann}
            deriving (Eq, Ord, Show)

makeLenses ''Ann

makeWrapped ''LocTerm

instance AsTermF LocTerm Ann where
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

stripLoc :: Term -> Term
stripLoc = Term . stripLoc' . unTerm

stripLoc' :: TermF Term -> TermF Term
stripLoc' (TmLoc _ t) = unTerm (stripLoc t) 
stripLoc' t = fmap stripLoc t

termToAnn :: Term -> Ann
termToAnn (Term (TmLoc l t)) = Ann (termToLocTerm t) (Just l)
termToAnn (Term t) = Ann (termToLocTerm (Term t)) Nothing

termToLocTerm :: Term -> LocTerm
termToLocTerm = LocTerm . fmap termToAnn . unTerm

annToTerm :: Ann -> Term
annToTerm (Ann t (Just l)) = Term (TmLoc l (locTermToTerm t))
annToTerm (Ann t Nothing) = locTermToTerm t

locTermToTerm :: LocTerm -> Term
locTermToTerm = Term . fmap annToTerm . unLocTerm

-- pattern matching

data Match s a = Match { match :: s -> Maybe a }

mBool :: AsTermF f g => Match f Bool
mBool = Match $ preview _TmBool

mTrue :: AsTermF f g => Match f ()
mTrue = Match $ \t -> match mBool t >>= \b -> if b then Just () else Nothing

mFalse :: AsTermF f g =>  Match f ()
mFalse = Match $ \t -> match mBool t >>= \b -> if b then Nothing else Just ()

-- TODO possibly return these in eot form
mIf :: AsTermF f g => Match f (g, g, g)
mIf = Match $ preview _TmIf

mInt :: AsTermF f g => Match f Int
mInt = Match $ preview _TmInt

mAdd :: AsTermF f g => Match f (g, g)
mAdd = Match $ preview _TmAdd

mMul :: AsTermF f g => Match f (g, g)
mMul = Match $ preview _TmMul

mEq :: AsTermF f g => Match f (g, g)
mEq = Match $ preview _TmEq

mLoc :: AsTermF f g => Match f (Loc, g)
mLoc = Match $ preview _TmLoc

data Type = TyBool
          | TyInt
          -- | TyArr Type Type
          deriving (Eq, Ord, Show)

-- eventually we want to use this in the gen / shrink / infer code

{-
tTrue :: Type
tTrue = TyBool

tFalse :: Type
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

data TypeError = UnexpectedType { expected :: Type, actual :: TypeInfo}
               | Mismatch {type1 :: TypeInfo, type2 :: TypeInfo}
               | UnknownType
               deriving (Eq, Ord, Show)

expect :: Type -> TypeInfo -> Either TypeError ()
expect ex t@(TypeInfo act _) =
  if ex == act
     then Right ()
     else Left $ UnexpectedType ex t

expectEq :: TypeInfo -> TypeInfo -> Either TypeError ()
expectEq t1@(TypeInfo u1 _) t2@(TypeInfo u2 _) =
  if u1 == u2
     then Right ()
     else Left $ Mismatch t1 t2

inferBool :: Ann -> Maybe (Either TypeError TypeInfo)
inferBool (Ann t l) = fmap (inferBool' l) . match mBool $ t

inferBool' :: Maybe Loc -> Bool -> Either TypeError TypeInfo
inferBool' = const . Right . TypeInfo TyBool

inferIf :: (Ann -> Either TypeError TypeInfo) -> Ann -> Maybe (Either TypeError TypeInfo)
inferIf inf (Ann t l) = fmap (inferIf' inf l) . match mIf $ t

-- converting the terms to types as they pass through would be useful
-- possibly would need TermF Term and TermF Type to get that done
inferIf' :: (Ann -> Either TypeError TypeInfo) -> Maybe Loc -> (Ann, Ann, Ann) -> Either TypeError TypeInfo
inferIf' inf l (tm1, tm2, tm3) = do
    ty1 <- inf tm1
    expect TyBool ty1
    ty2@(TypeInfo t _) <- inf tm2
    ty3 <- inf tm3
    expectEq ty2 ty3
    return (TypeInfo t l)

inferInt :: Ann -> Maybe (Either TypeError TypeInfo)
inferInt (Ann t l) = fmap (inferInt' l) . match mInt $ t

inferInt' :: Maybe Loc -> Int -> Either TypeError TypeInfo
inferInt' = const . Right . TypeInfo TyInt

inferAdd :: (Ann -> Either TypeError TypeInfo) -> Ann -> Maybe (Either TypeError TypeInfo)
inferAdd inf (Ann t l) = fmap (inferAdd' inf l) . match mAdd $ t

inferAdd' :: (Ann -> Either TypeError TypeInfo) -> Maybe Loc -> (Ann, Ann) -> Either TypeError TypeInfo
inferAdd' inf l (tm1, tm2) = do
    ty1 <- inf tm1
    expect TyInt ty1
    ty2 <- inf tm2
    expect TyInt ty2
    return $ TypeInfo TyInt l

inferMul :: (Ann -> Either TypeError TypeInfo) -> Ann -> Maybe (Either TypeError TypeInfo)
inferMul inf (Ann t l) = fmap (inferMul' inf l) . match mMul $ t

inferMul' :: (Ann -> Either TypeError TypeInfo) -> Maybe Loc -> (Ann, Ann) -> Either TypeError TypeInfo
inferMul' inf l (tm1, tm2) = do
    ty1 <- inf tm1
    expect TyInt ty1
    ty2 <- inf tm2
    expect TyInt ty2
    return $ TypeInfo TyInt l

inferEq :: (Ann -> Either TypeError TypeInfo) -> Ann -> Maybe (Either TypeError TypeInfo)
inferEq inf (Ann t l) = fmap (inferEq' inf l) . match mEq $ t

inferEq' :: (Ann -> Either TypeError TypeInfo) -> Maybe Loc -> (Ann, Ann) -> Either TypeError TypeInfo
inferEq' inf l (tm1, tm2) = do
    ty1 <- inf tm1
    expect TyInt ty1
    ty2 <- inf tm2
    expect TyInt ty2
    return $ TypeInfo TyBool l

inferLoc :: (Ann -> Either TypeError TypeInfo) -> Ann -> Maybe (Either TypeError TypeInfo)
inferLoc inf (Ann t l) = fmap (inferLoc' inf l) . match mLoc $ t

inferLoc' :: (Ann -> Either TypeError TypeInfo) -> Maybe Loc -> (Loc, Ann) -> Either TypeError TypeInfo
inferLoc' inf _ (_, t) = inf t

infer :: Ann -> Either TypeError TypeInfo
infer a =
  fromMaybe (Left UnknownType) .
  asum .
  map ($ a) $
    [ inferBool
    , inferIf infer
    , inferInt
    , inferAdd infer
    , inferMul infer
    , inferEq infer
    , inferLoc infer
    ]

-- values

bv :: Term -> Maybe Term
bv t = t <$ preview _TmBool t

nv :: Term -> Maybe Term
nv t = t <$ preview _TmInt t

v :: Term -> Maybe Term
v t = asum [bv t, nv t]

-- small step semantics

sStepIfTrue :: Term -> Maybe Term
sStepIfTrue t = match mIf t >>= \(b, c1, _) -> match mTrue b >>= \_ -> return c1

sStepIfFalse :: Term -> Maybe Term
sStepIfFalse t = match mIf t >>= \(b, _, c2) -> match mFalse b >>= \_ -> return c2

sStepIf :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepIf step t = match mIf t >>= \(b, c1, c2) -> step b >>= \b' -> return (review _TmIf (b', c1, c2))

sStepAddIntInt :: Term -> Maybe Term
sStepAddIntInt t = match mAdd t >>= \(t1, t2) -> match mInt t1 >>= \i1 -> match mInt t2 >>= \i2 -> return (review _TmInt (i1 + i2))

sStepAdd1 :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepAdd1 step t = match mAdd t >>= \(t1, t2) -> step t1 >>= \u1 -> return (review _TmAdd (u1, t2))

sStepAdd2 :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepAdd2 step t = match mAdd t >>= \(t1, t2) -> match mInt t1 >>= \_ -> step t2 >>= \u2 -> return (review _TmAdd (t1, u2))

sStepMulIntInt :: Term -> Maybe Term
sStepMulIntInt t = match mMul t >>= \(t1, t2) -> match mInt t1 >>= \i1 -> match mInt t2 >>= \i2 -> return (review _TmInt (i1 * i2))

sStepMul1 :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepMul1 step t = match mMul t >>= \(t1, t2) -> step t1 >>= \u1 -> return (review _TmMul (u1, t2))

sStepMul2 :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepMul2 step t = match mMul t >>= \(t1, t2) -> match mInt t1 >>= \_ -> step t2 >>= \u2 -> return (review _TmMul (t1, u2))

sStepEqIntInt :: Term -> Maybe Term
sStepEqIntInt t = match mEq t >>= \(t1, t2) -> match mInt t1 >>= \i1 -> match mInt t2 >>= \i2 -> return (review _TmBool (i1 == i2))

sStepEq1 :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepEq1 step t = match mEq t >>= \(t1, t2) -> step t1 >>= \u1 -> return (review _TmEq (u1, t2))

sStepEq2 :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepEq2 step t = match mEq t >>= \(t1, t2) -> match mInt t1 >>= \_ -> step t2 >>= \u2 -> return (review _TmEq (t1, u2))

sStepLoc :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepLoc step t = match mLoc t >>= \(_, u) -> step u

sStep :: Term -> Maybe Term
sStep t = asum . map ($ t) $ [
    sStepIfTrue
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
sEval :: Term -> Term
sEval t = case sStep t of
  Nothing -> t
  Just u -> sEval u

-- big step semantics

-- https://www.cs.ubc.ca/~rxg/cpsc509/04-big-step.pdf
-- https://fos2015.github.io/project1.html

bStepIfTrue :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepIfTrue step t = match mIf t >>= \(b, c1, _) -> step b >>= match mTrue >>= \_ -> step c1

bStepIfFalse :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepIfFalse step t = match mIf t >>= \(b, _, c2) -> step b >>= match mFalse >>= \_ -> step c2

bStepAdd :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepAdd step t = match mAdd t >>= \(t1, t2) -> step t1 >>= match mInt >>= \i1 -> step t2 >>= match mInt >>= \i2 -> return (review _TmInt (i1 + i2))

bStepMul :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepMul step t = match mMul t >>= \(t1, t2) -> step t1 >>= match mInt >>= \i1 -> step t2 >>= match mInt >>= \i2 -> return (review _TmInt (i1 * i2))

bStepEq :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepEq step t = match mEq t >>= \(t1, t2) -> step t1 >>= match mInt >>= \i1 -> step t2 >>= match mInt >>= \i2 -> return (review _TmBool (i1 == i2))

bStepLoc :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepLoc step t = match mLoc t >>= \(_, u) -> step u

bStep :: Term -> Maybe Term
bStep t = asum . map ($ t) $ [
      v
    , bStepIfTrue bStep
    , bStepIfFalse bStep
    , bStepAdd bStep
    , bStepMul bStep
    , bStepEq bStep
    , bStepLoc bStep
  ]

bEval :: Term -> Term
bEval t = fromMaybe t . bStep $ t

-- precedence

-- TODO, break up into pieces

data OperatorInfo = OperatorInfo { precedence :: Int, assoc :: Assoc }
                    deriving (Eq, Ord, Show)

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

precTerm :: Term -> Maybe OperatorInfo
precTerm t = asum [ match mAdd t >> precAdd , match mMul t >> precMul, match mEq t >> precEq]

-- printer

prBool :: Term -> Maybe Doc
prBool = fmap prBool' . match mBool

prBool' :: Bool -> Doc
prBool' True = text "true"
prBool' False = text "false"

prIf :: (Term -> Doc) -> Term -> Maybe Doc
prIf pr = fmap (prIf' pr) . match mIf

prIf' :: (Term -> Doc) -> (Term, Term, Term) -> Doc
prIf' pr (b, c1, c2) = text "if" <+> pr b </> text "then" <+> pr c1 </> text "else" <+> pr c2

prInt :: Term -> Maybe Doc
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

prAdd :: (Term -> Doc) -> Term -> Maybe Doc
prAdd pr = fmap (prAdd' pr) . match mAdd

prAdd' :: (Term -> Doc) -> (Term, Term) -> Doc
prAdd' pr (t1, t2) = pr t1 <+> text "+" <+> pr t2

prMul :: (Term -> Doc) -> Term -> Maybe Doc
prMul pr = fmap (prMul' pr) . match mMul

prMul' :: (Term -> Doc) -> (Term, Term) -> Doc
prMul' pr (t1, t2) = pr t1 <+> text "*" <+> pr t2

prEq :: (Term -> Doc) -> Term -> Maybe Doc
prEq pr = fmap (prEq' pr) . match mEq

prEq' :: (Term -> Doc) -> (Term, Term) -> Doc
prEq' pr (t1, t2) = pr t1 <+> text "==" <+> pr t2

prLoc :: (Term -> Doc) -> Term -> Maybe Doc
prLoc pr = fmap (prLoc' pr) . match mLoc

prLoc' :: (Term -> Doc) -> (Loc, Term) -> Doc
prLoc' pr (_, t) = pr t

data PrinterType t =
    RegularPrinter (t -> Maybe Doc)
  | OpPrinter (t -> Maybe (t, t)) (Doc -> Doc -> Doc)

opAdd :: PrinterType Term
opAdd = OpPrinter (match mAdd) (\x y -> x <+> text "+" <+> y)

opMul :: PrinterType Term
opMul = OpPrinter (match mMul) (\x y -> x <+> text "*" <+> y)

opEq :: PrinterType Term
opEq = OpPrinter (match mEq) (\x y -> x <+> text "==" <+> y)

printingTable :: [(Maybe OperatorInfo, PrinterType Term)]
printingTable = [
    (precBool, RegularPrinter prBool)
  , (precIf, RegularPrinter (prIf prettyTerm))
  , (precInt, RegularPrinter prInt)
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
printTerm :: Term -> Doc
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

prettyTerm :: Term -> Doc
prettyTerm t =
  fromMaybe PP.empty .
  asum .
  map ($ t) $ [
    prBool
  , prIf prettyTerm
  , prInt
  , prAdd prettyTerm
  , prMul prettyTerm
  , prEq prettyTerm
  , prLoc prettyTerm
  ]

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyString :: Term -> String
prettyString = docString . prettyTerm

-- parser

style :: PC.CharParsing m => IdentifierStyle m
style = IdentifierStyle "nb" PC.lower PC.alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["true", "false", "if", "then", "else", "+", "=="]

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

data ParserType m f = ParseRegular (m f) | ParseOp (m (f -> f -> f))

parseBool :: (Monad m, TokenParsing m) => m Term
parseBool = fmap (review _TmBool) (parseBool' T.<?> "bool")
  where
    parseBool' =
      True <$ reserved "true" <|>
      False <$ reserved "false"

parseIf :: (Monad m, TokenParsing m) => m Term -> m Term
parseIf p = fmap (review _TmIf) ((,,) <$ reserved "if" <*> p <* reserved "then" <*> p <* reserved "else" <*> p T.<?> "if-then-else")

parseInt :: (Monad m, TokenParsing m) => m Term
parseInt = fmap (review _TmInt . fromInteger) (T.integer T.<?> "int")

parseAdd :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseAdd = curry (review _TmAdd) <$ reserved "+"

parseMul :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseMul = curry (review _TmMul) <$ reserved "*"

parseEq :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseEq = curry (review _TmEq) <$ reserved "=="

parseLoc :: (Monad m, T.DeltaParsing m) => m Term -> m Term
parseLoc p = do
  (t :~ l) <- T.spanned p
  return $ review _TmLoc (l, t)

parsingTable :: (Monad m, T.DeltaParsing m) => [(Maybe OperatorInfo, ParserType m Term)]
parsingTable = [
    (precBool, ParseRegular parseBool)
  , (precIf, ParseRegular (parseIf parseTerm))
  , (precInt, ParseRegular parseInt)
  , (precAdd, ParseOp parseAdd)
  , (precMul, ParseOp parseMul)
  , (precEq, ParseOp parseEq)
  ]

-- can build the table by grabbing the precedence associated with
-- each parser

-- no precednce -> term parser
-- has precedence -> expression parser

parseTerm :: (Monad m, T.DeltaParsing m) => m Term
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

parseString :: String -> Either String Term
parseString s = case T.parseString parseTerm (Lines 0 0 0 0) s of
  T.Success r -> Right r
  T.Failure d -> Left (docString d)

-- gen and shrink

gBool :: Gen Term
gBool = fmap (review _TmBool) arbitrary

gIf :: Gen Term -> Gen Term -> Gen Term
gIf t1 t2 = fmap (review _TmIf) ((,,) <$> t1 <*> t2 <*> t2)

gInt :: Gen Term
gInt = fmap (review _TmInt) arbitrary

gAdd :: Gen Term -> Gen Term
gAdd t = fmap (review _TmAdd) ((,) <$> t <*> t)

gMul :: Gen Term -> Gen Term
gMul t = fmap (review _TmMul) ((,) <$> t <*> t)

gEq :: Gen Term -> Gen Term
gEq t = fmap (review _TmEq) ((,) <$> t <*> t)

genAny :: Gen Term
genAny = sized genAny'

genAny' :: Int -> Gen Term
genAny' s =
    oneof $ zeroSize ++ if s == 0 then [] else otherSize
  where
    zeroSize = [gBool, gInt]
    child = genAny' (s `div` 2)
    otherSize = [gIf child child, gAdd child, gMul child, gEq child]

genTerm :: Gen Term
genTerm = sized $ genTerm' Nothing

genTerm' :: Maybe Type -> Int -> Gen Term
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

-- keep things well typed in shrinking
-- - we have gens for dealing with ill typed stuff

sBool :: Term -> Maybe [Term]
sBool = fmap sBool' . match mBool

sBool' :: Bool -> [Term]
sBool' = const []

sIf :: (Term -> [Term]) -> Term -> Maybe [Term]
sIf shr = fmap (sIf' shr) . match mIf

sIf' :: (Term -> [Term]) -> (Term, Term, Term) -> [Term]
sIf' shr (t1, t2, t3) =
  shr t2 ++
  shr t3 ++
  fmap (\t -> review _TmIf (t, t2, t3)) (shr t1) ++
  fmap (\t -> review _TmIf (t1, t, t3)) (shr t2) ++
  fmap (\t -> review _TmIf (t1, t2, t)) (shr t3)

sInt :: Term -> Maybe [Term]
sInt = fmap sInt' . match mInt

sInt' :: Int -> [Term]
sInt' = const []

sAdd :: (Term -> [Term]) -> Term -> Maybe [Term]
sAdd shr = fmap (sAdd' shr) . match mAdd

sAdd' :: (Term -> [Term]) -> (Term, Term) -> [Term]
sAdd' shr (t1, t2) =
  shr t1 ++
  shr t2 ++
  fmap (\t -> review _TmAdd (t, t2)) (shr t1) ++
  fmap (\t -> review _TmAdd (t1, t)) (shr t2)

sMul :: (Term -> [Term]) -> Term -> Maybe [Term]
sMul shr = fmap (sMul' shr) . match mMul

sMul' :: (Term -> [Term]) -> (Term, Term) -> [Term]
sMul' shr (t1, t2) =
  shr t1 ++
  shr t2 ++
  fmap (\t -> review _TmMul (t, t2)) (shr t1) ++
  fmap (\t -> review _TmMul (t1, t)) (shr t2)

sEq :: (Term -> [Term]) -> Term -> Maybe [Term]
sEq shr = fmap (sEq' shr) . match mEq

sEq' :: (Term -> [Term]) -> (Term, Term) -> [Term]
sEq' shr (t1, t2) = 
  fmap (\t -> review _TmEq (t, t2)) (shr t1) ++
  fmap (\t -> review _TmEq (t1, t)) (shr t2)

sLoc :: (Term -> [Term]) -> Term -> Maybe [Term]
sLoc shr = fmap (sLoc' shr) . match mLoc

sLoc' :: (Term -> [Term]) -> (Loc, Term) -> [Term]
sLoc' shr (s, t) = fmap (\t -> review _TmLoc (s,t)) (shr t)

shrinkTerm :: Term -> [Term]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    sBool
  , sIf shrinkTerm
  , sInt
  , sAdd shrinkTerm
  , sMul shrinkTerm
  , sEq shrinkTerm
  , sLoc shrinkTerm
  ]

instance Arbitrary Term where
  arbitrary = genTerm
  shrink = shrinkTerm

newtype IllTerm = IllTerm { runIll :: Term }
                  deriving (Eq, Ord, Show)

instance Arbitrary IllTerm where
   arbitrary = IllTerm <$> genAny
   shrink = fmap IllTerm . shrinkTerm . runIll

newtype SrcTerm = SrcTerm { runSrc :: Term}
                  deriving (Eq, Ord, Show)

instance Arbitrary SrcTerm where
  arbitrary = (SrcTerm . fromRight . parseString . prettyString) <$> genTerm
    where
      fromRight (Right x) = x
  shrink = fmap SrcTerm . shrinkTerm . runSrc

newtype SrcIllTerm = SrcIllTerm { runSrcIll :: Term}
                  deriving (Eq, Ord, Show)

instance Arbitrary SrcIllTerm where
  arbitrary = (SrcIllTerm . fromRight . parseString . prettyString) <$> genAny
    where
      fromRight (Right x) = x
  shrink = fmap SrcIllTerm . shrinkTerm . runSrcIll

propWellTyped :: Term -> Bool
propWellTyped = isRight . infer . termToAnn

propSmallBig :: Term -> Bool
propSmallBig = (==) <$> sEval <*> bEval

-- we should be able test this for each of the individual steps
-- concretely:
-- - the gen for each step should behave the same on
--     - the rule for the step
--     - the overal step function
propPreservation :: Term -> Bool
propPreservation t =
  case sStep t of
    Nothing -> True
    Just u -> infer (termToAnn t) == infer (termToAnn u)

propProgress :: Term -> Bool
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

propIll :: IllTerm -> Bool
propIll (IllTerm t) =
  isJust (v t) ||
  isJust (sStep t) ||
  isLeft (infer (termToAnn t))

propRoundTrip :: Term -> Property
propRoundTrip t = propRoundTrip' t
  where
    propRoundTrip' = (===) <$> Right <*> (fmap stripLoc . parseString . prettyString)
