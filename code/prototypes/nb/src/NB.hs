{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module NB where

import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Either (isRight, isLeft)
import Data.Foldable (asum)
import Control.Lens
import Test.QuickCheck

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Text.Parser.Char as PC
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS
import qualified Text.Trifecta as T
import Text.Trifecta.Delta
import Text.Trifecta.Rendering

-- TODO, break up, use classy prisms to link back to main term
data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | TmLoc T.Span Term
          deriving (Eq, Ord, Show)

makeClassyPrisms ''Term

-- pattern matching

data Match s a = Match { match :: s -> Maybe a }

mTrue :: Match Term ()
mTrue = Match $ \t -> case t of
    TmTrue -> Just ()
    _ -> Nothing

mFalse :: Match Term ()
mFalse = Match $ \t -> case t of
    TmFalse -> Just ()
    _ -> Nothing

-- TODO possibly return these in eot form
mIf :: Match Term (Term, Term, Term)
mIf = Match $ \t -> case t of
    TmIf t1 t2 t3 -> Just (t1, t2, t3)
    _ -> Nothing

mZero :: Match Term ()
mZero = Match $ \t -> case t of
    TmZero -> Just ()
    _ -> Nothing

mSucc :: Match Term Term
mSucc = Match $ \t -> case t of 
  TmSucc t1 -> Just t1
  _ -> Nothing

mPred :: Match Term Term
mPred = Match $ \t -> case t of 
  TmPred t1 -> Just t1
  _ -> Nothing

mIsZero :: Match Term Term
mIsZero = Match $ \t -> case t of
  TmIsZero t1 -> Just t1
  _ -> Nothing

mLoc :: Match Term (T.Span, Term)
mLoc = Match $ \t -> case t of
  TmLoc l t1 -> Just (l, t1)
  _ -> Nothing

data Type = TyBool
          | TyNat
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

-- TODO pass span into TypeError?
-- we can then convert to a good Doc?
-- or use Text.Trifecta.Result (Err)

data TypeError = UnexpectedType { expected :: Type, actual :: Type}
               | Mismatch {type1 :: Type, type2 :: Type}
               | UnknownType
               deriving (Eq, Ord, Show)

expect :: Type -> Type -> Either TypeError ()
expect ex act =
  if ex == act
     then Right ()
     else Left $ UnexpectedType ex act

expectEq :: Type -> Type -> Either TypeError ()
expectEq t1 t2 =
  if t1 == t2
     then Right ()
     else Left $ Mismatch t1 t2

inferTrue :: Term -> Maybe (Either TypeError Type)
inferTrue = fmap inferTrue' . match mTrue

inferTrue' :: () -> Either TypeError Type
inferTrue' _ = Right TyBool

inferFalse :: Term -> Maybe (Either TypeError Type)
inferFalse = fmap inferFalse' . match mFalse

inferFalse' :: () -> Either TypeError Type
inferFalse' = const $ Right TyBool

inferIf :: (Term -> Either TypeError Type) -> Term -> Maybe (Either TypeError Type)
inferIf inf = fmap (inferIf' inf) . match mIf

-- converting the terms to types as they pass through would be useful
-- possibly would need TermF Term and TermF Type to get that done
inferIf' :: (Term -> Either TypeError Type) -> (Term, Term, Term) -> Either TypeError Type
inferIf' inf (tm1, tm2, tm3) = do
    ty1 <- inf tm1
    expect TyBool ty1
    ty2 <- inf tm2
    ty3 <- inf tm3
    expectEq ty2 ty3
    return ty2

inferZero :: Term -> Maybe (Either TypeError Type)
inferZero = fmap inferZero' . match mZero

inferZero' :: () -> Either TypeError Type
inferZero' = const $ Right TyNat

inferSucc :: (Term -> Either TypeError Type) -> Term -> Maybe (Either TypeError Type)
inferSucc inf = fmap (inferSucc' inf) . match mSucc

inferSucc' :: (Term -> Either TypeError Type) -> Term -> Either TypeError Type
inferSucc' inf tm = do
    ty <- inf tm
    expect TyNat ty
    return TyNat

inferPred :: (Term -> Either TypeError Type) -> Term -> Maybe (Either TypeError Type)
inferPred inf = fmap (inferPred' inf) . match mPred

inferPred' :: (Term -> Either TypeError Type) -> Term -> Either TypeError Type
inferPred' inf tm = do
    ty <- inf tm
    expect TyNat ty
    return TyNat

inferIsZero :: (Term -> Either TypeError Type) -> Term -> Maybe (Either TypeError Type)
inferIsZero inf = fmap (inferIsZero' inf) . match mIsZero

inferIsZero' :: (Term -> Either TypeError Type) -> Term -> Either TypeError Type
inferIsZero' inf tm = do
    ty <- inf tm
    expect TyNat ty
    return TyBool

inferLoc :: (Term -> Either TypeError Type) -> Term -> Maybe (Either TypeError Type)
inferLoc inf = fmap (inferLoc' inf) . match mLoc

inferLoc' :: (Term -> Either TypeError Type) -> (T.Span, Term) -> Either TypeError Type
inferLoc' inf (_, t) = inf t

infer :: Term -> Either TypeError Type
infer t =
  fromMaybe (Left UnknownType) .
  asum .
  map ($ t) $
    [ inferTrue
    , inferFalse
    , inferIf infer
    , inferZero
    , inferSucc infer
    , inferPred infer
    , inferIsZero infer
    , inferLoc infer
    ]

-- values

bv :: Term -> Maybe Term
bv TmTrue = Just TmTrue
bv TmFalse = Just TmFalse
bv _ = Nothing

nv :: Term -> Maybe Term
nv TmZero = Just TmZero
nv (TmSucc x) = fmap TmSucc (nv x)
nv _ = Nothing

v :: Term -> Maybe Term
v t = asum [bv t, nv t]

-- small step semantics

sStepIfTrue :: Term -> Maybe Term
sStepIfTrue t = match mIf t >>= \(b, c1, _) -> match mTrue b >>= \_ -> return c1

sStepIfFalse :: Term -> Maybe Term
sStepIfFalse t = match mIf t >>= \(b, _, c2) -> match mFalse b >>= \_ -> return c2

sStepIf :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepIf step t = match mIf t >>= \(b, c1, c2) -> step b >>= \b' -> return (TmIf b' c1 c2)

sStepSucc :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepSucc step t = fmap TmSucc (match mSucc t >>= step)

sStepPredZero :: Term -> Maybe Term
sStepPredZero t = match mPred t >>= match mZero >>= \_ -> return TmZero

sStepPredSucc :: Term -> Maybe Term
sStepPredSucc t = match mPred t >>= match mSucc >>= nv

sStepPred :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepPred step t = fmap TmPred (match mPred t >>= step)

sStepIsZeroZero :: Term -> Maybe Term
sStepIsZeroZero t = match mIsZero t >>= match mZero >>= \_ -> return TmTrue

sStepIsZeroSucc :: Term -> Maybe Term
sStepIsZeroSucc t = match mIsZero t >>= match mSucc >>= nv >>= \_ -> return TmFalse

sStepIsZero :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepIsZero step t = fmap TmIsZero (match mIsZero t >>= step)

sStepLoc :: (Term -> Maybe Term) -> Term -> Maybe Term
sStepLoc step t = match mLoc t >>= \(_, u) -> step u

sStep :: Term -> Maybe Term
sStep t = asum . map ($ t) $ [
    sStepIfTrue
  , sStepIfFalse
  , sStepIf sStep
  , sStepSucc sStep
  , sStepPred sStep
  , sStepPredZero
  , sStepPredSucc
  , sStepIsZeroZero
  , sStepIsZeroSucc
  , sStepIsZero sStep
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

-- need big step semantics for reducing TmSucc (non-value)
bStepSucc :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepSucc step t = fmap TmSucc (match mSucc t >>= step)

bStepPredZero :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepPredZero step t = match mPred t >>= step >>= match mZero >>= \_ -> return TmZero

bStepPredSucc :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepPredSucc step t = match mPred t >>= step >>= match mSucc

bStepIsZeroZero :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepIsZeroZero step t = match mIsZero t >>= step >>= match mZero >>= \_ -> return TmTrue

bStepIsZeroSucc :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepIsZeroSucc step t = match mIsZero t >>= step >>= match mSucc >>= \_ -> return TmFalse

bStepLoc :: (Term -> Maybe Term) -> Term -> Maybe Term
bStepLoc step t = match mLoc t >>= \(_, u) -> step u

bStep :: Term -> Maybe Term
bStep t = asum . map ($ t) $ [
      v
    , bStepIfTrue bStep
    , bStepIfFalse bStep
    , bStepSucc bStep
    , bStepPredZero bStep
    , bStepPredSucc bStep
    , bStepIsZeroZero bStep
    , bStepIsZeroSucc bStep
    , bStepLoc bStep
  ]

bEval :: Term -> Term
bEval t = fromMaybe t . bStep $ t

-- printer

prTrue :: Term -> Maybe Doc
prTrue = fmap prTrue' . match mTrue

prTrue' :: () -> Doc
prTrue' = const $ text "true"

prFalse :: Term -> Maybe Doc
prFalse = fmap prFalse' . match mFalse

prFalse' :: () -> Doc
prFalse' = const $ text "false"

prIf :: (Term -> Doc) -> Term -> Maybe Doc
prIf pr = fmap (prIf' pr) . match mIf

prIf' :: (Term -> Doc) -> (Term, Term, Term) -> Doc
prIf' pr (b, c1, c2) = text "if" <+> pr b </> text "then" <+> pr c1 </> text "else" <+> pr c2

prZero :: Term -> Maybe Doc
prZero = fmap prZero' . match mZero

prZero' :: () -> Doc
prZero' = const $ text "O"

prSucc :: (Term -> Doc) -> Term -> Maybe Doc
prSucc pr = fmap (prSucc' pr) . match mSucc

prSucc' :: (Term -> Doc) -> Term -> Doc
prSucc' pr t = text "S" <+> pr t

prPred :: (Term -> Doc) -> Term -> Maybe Doc
prPred pr = fmap (prPred' pr) . match mPred

prPred' :: (Term -> Doc) -> Term -> Doc
prPred' pr t = text "pred" <+> pr t

prIsZero :: (Term -> Doc) -> Term -> Maybe Doc
prIsZero pr = fmap (prIsZero' pr) . match mIsZero

prIsZero' :: (Term -> Doc) -> Term -> Doc
prIsZero' pr t = text "isZero" <+> pr t

prLoc :: (Term -> Doc) -> Term -> Maybe Doc
prLoc pr = fmap (prLoc' pr) . match mLoc

prLoc' :: (Term -> Doc) -> (T.Span, Term) -> Doc
prLoc' pr (_, t) = pr t

prettyTerm :: Term -> Doc
prettyTerm t =
  fromMaybe empty .
  asum .
  map ($ t) $ [
    prTrue
  , prFalse
  , prIf prettyTerm
  , prZero
  , prSucc prettyTerm
  , prPred prettyTerm
  , prIsZero prettyTerm
  , prLoc prettyTerm
  ]

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyString :: Term -> String
prettyString = docString . prettyTerm

-- parser

-- TODO name all of the parsers with <?>

style :: PC.CharParsing m => IdentifierStyle m
style = IdentifierStyle "nb" PC.lower PC.alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["true", "false", "if", "then", "else", "O", "S", "pred", "isZero"]

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseTrue :: (Monad m, TokenParsing m) => m Term
parseTrue = TmTrue <$ reserved "true" T.<?> "true"

parseFalse :: (Monad m, TokenParsing m) => m Term
parseFalse = TmFalse <$ reserved "false" T.<?> "false"

parseIf :: (Monad m, TokenParsing m) => m Term -> m Term
parseIf p = TmIf <$ reserved "if" <*> p <* reserved "then" <*> p <* reserved "else" <*> p T.<?> "if-then-else"

parseZero :: (Monad m, TokenParsing m) => m Term
parseZero = TmZero <$ reserved "O" T.<?> "zero"

parseSucc :: (Monad m, TokenParsing m) => m Term -> m Term
parseSucc p = TmSucc <$ reserved "S" <*> p T.<?> "succ"

parsePred :: (Monad m, TokenParsing m) => m Term -> m Term
parsePred p = TmPred <$ reserved "pred" <*> p T.<?> "pred"

parseIsZero :: (Monad m, TokenParsing m) => m Term -> m Term
parseIsZero p = TmIsZero <$ reserved "isZero" <*> p T.<?> "isZero"

parseLoc :: (Monad m, T.DeltaParsing m) => m Term -> m Term
parseLoc p = do
  (t :~ l) <- T.spanned p
  return $ TmLoc l t

parseTerm :: (Monad m, T.DeltaParsing m) => m Term
parseTerm = asum . fmap parseLoc $ [
    parseTrue
  , parseFalse
  , parseIf parseTerm
  , parseZero
  , parseSucc parseTerm
  , parsePred parseTerm
  , parseIsZero parseTerm
  ]

parseString :: String -> Either String Term
parseString s = case T.parseString parseTerm (Lines 0 0 0 0) s of
  T.Success r -> Right r
  T.Failure d -> Left (docString d)

-- gen and shrink

gTrue :: Gen Term
gTrue = pure TmTrue

gFalse :: Gen Term
gFalse = pure TmFalse

gIf :: Gen Term -> Gen Term -> Gen Term
gIf t1 t2 = TmIf <$> t1 <*> t2 <*> t2

gZero :: Gen Term
gZero = pure TmZero

gSucc :: Gen Term -> Gen Term
gSucc t = TmSucc <$> t

gPred :: Gen Term -> Gen Term
gPred t = TmPred <$> t

gIsZero :: Gen Term -> Gen Term
gIsZero t = TmIsZero <$> t

genAny :: Gen Term
genAny = sized genAny'

genAny' :: Int -> Gen Term
genAny' s =
    oneof $ zeroSize ++ if s == 0 then [] else otherSize
  where
    zeroSize = [gTrue, gFalse, gZero]
    child = genAny' (s `div` 2)
    otherSize = [gIf child child, gSucc child, gPred child, gIsZero child]

genBool :: Gen Term
genBool = sized $ genTerm' (Just TyBool)

genNat :: Gen Term
genNat = sized $ genTerm' (Just TyNat)

genTerm :: Gen Term
genTerm = sized $ genTerm' Nothing

genTerm' :: Maybe Type -> Int -> Gen Term
genTerm' Nothing s =
  oneof [
    genTerm' (Just TyBool) s
  , genTerm' (Just TyNat) s
  ]
genTerm' (Just TyNat) s =
  let
    zeroSize = [gZero]
    s' = s `div` 2
    gB = genTerm' (Just TyBool) s'
    gN = genTerm' (Just TyNat) s'
    otherSize = [gIf gB gN, gSucc gN, gPred gN]
  in
    oneof $ zeroSize ++ if s == 0 then [] else otherSize
genTerm' (Just TyBool) s =
  let
    zeroSize = [gTrue, gFalse]
    s' = s `div` 2
    gB = genTerm' (Just TyBool) s'
    gN = genTerm' (Just TyNat) s'
    otherSize = [gIsZero gN, gIf gB gB]
  in
    oneof $ zeroSize ++ if s == 0 then [] else otherSize

-- keep things well typed in shrinking
-- - we have gens for dealing with ill typed stuff

sTrue :: Term -> Maybe [Term]
sTrue = fmap sTrue' . match mTrue

sTrue' :: () -> [Term]
sTrue' = const []

sFalse :: Term -> Maybe [Term]
sFalse = fmap sFalse' . match mFalse

sFalse' :: () -> [Term]
sFalse' = const []

sIf :: (Term -> [Term]) -> Term -> Maybe [Term]
sIf shr = fmap (sIf' shr) . match mIf

sIf' :: (Term -> [Term]) -> (Term, Term, Term) -> [Term]
sIf' shr (t1, t2, t3) =
  shr t2 ++
  shr t3 ++
  fmap (\t -> TmIf t t2 t3) (shr t1) ++
  fmap (\t -> TmIf t1 t t3) (shr t2) ++
  fmap (TmIf t1 t2) (shr t3)

sZero :: Term -> Maybe [Term]
sZero = fmap sZero' . match mZero

sZero' :: () -> [Term]
sZero' = const []

sSucc :: (Term -> [Term]) -> Term -> Maybe [Term]
sSucc shr = fmap (sSucc' shr) . match mSucc

sSucc' :: (Term -> [Term]) -> Term -> [Term]
sSucc' shr t = shr t ++ t : fmap TmSucc (shr t)

sPred :: (Term -> [Term]) -> Term -> Maybe [Term]
sPred shr = fmap (sPred' shr) . match mPred

sPred' :: (Term -> [Term]) -> Term -> [Term]
sPred' shr t = shr t ++ t : fmap TmPred (shr t)

sIsZero :: (Term -> [Term]) -> Term -> Maybe [Term]
sIsZero shr = fmap (sIsZero' shr) . match mIsZero

sIsZero' :: (Term -> [Term]) -> Term -> [Term]
sIsZero' shr t = fmap TmIsZero (shr t)

sLoc :: (Term -> [Term]) -> Term -> Maybe [Term]
sLoc shr = fmap (sLoc' shr) . match mLoc

sLoc' :: (Term -> [Term]) -> (T.Span, Term) -> [Term]
sLoc' shr (s, t) = fmap (TmLoc s) (shr t)

shrinkTerm :: Term -> [Term]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    sTrue
  , sFalse
  , sIf shrinkTerm
  , sZero
  , sSucc shrinkTerm
  , sPred shrinkTerm
  , sIsZero shrinkTerm
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

newtype LocTerm = LocTerm { runLoc :: Term}
                  deriving (Eq, Ord, Show)

instance Arbitrary LocTerm where
  arbitrary = (LocTerm . fromRight . parseString . prettyString) <$> genAny
    where
      fromRight (Right x) = x
  shrink = fmap LocTerm . shrinkTerm . runLoc

propWellTyped :: Term -> Bool
propWellTyped = isRight . infer

propSmallBig :: Term -> Bool
propSmallBig = (==) <$> sEval <*> bEval

propPreservation :: Term -> Bool
propPreservation t = case sStep t of
  Nothing -> True
  Just u -> infer t == infer u

propProgress :: Term -> Bool
propProgress t = case v t of
  Just _ -> True
  Nothing -> isJust (sStep t)

propIll :: IllTerm -> Bool
propIll (IllTerm t) =
  isJust (v t) ||
  isJust (sStep t) ||
  isLeft (infer t)

propRoundTrip :: LocTerm -> Property
propRoundTrip (LocTerm t) = propRoundTrip' t
  where
    propRoundTrip' = (===) <$> Right <*> (parseString . prettyString)
