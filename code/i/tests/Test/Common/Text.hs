{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Common.Text where
--(
--    textTests
--  ) where

-- from 'tasty'
import           Test.Tasty                   (TestTree, testGroup)

-- from 'tasty-quickcheck'
import           Test.Tasty.QuickCheck        (testProperty)

-- from 'QuickCheck'
import           Test.QuickCheck              (Arbitrary (..), Gen, Property,
                                               elements, forAllShrink, listOf1,
                                               oneof, property, shrinkList,
                                               sized, (===))
-- from 'containers'
import qualified Data.Map                     as M (Map, fromList, keys, toList)

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc, int, text, (<+>))

-- from 'parsers'
import           Text.Parser.Token            (TokenParsing, integer, symbol)

-- local
import           Common.Parse                 (ParseRule (..), mkParser,
                                               parseFromString)
import           Common.Pretty                (PrettyRule (..), mkPretty,
                                               prettyToString)
import           Common.Text                  (OperatorInfo)

-- TODO gather properties
textTests :: TestTree
textTests = testGroup "text"
  [ testProperty "pretty-parse round trip" propPrettyParse
  , testProperty "pretty-parse correct parens" propPrettyParseParens
  ]

newtype OperatorMap = OperatorMap {
    getOperatorMap :: M.Map String OperatorInfo
  } deriving (Eq, Show)

genOperatorEntry :: Gen (String, OperatorInfo)
genOperatorEntry =
  (,) <$> elements (map pure "+=*&^%$#@!") <*> arbitrary

genOperatorMap :: Gen OperatorMap
genOperatorMap =
  fmap (OperatorMap . M.fromList) .
  listOf1 $
  genOperatorEntry

shrinkOperatorEntry :: (String, OperatorInfo)
                    -> [(String, OperatorInfo)]
shrinkOperatorEntry (s, oi) =
  fmap (\oi' -> (s, oi')) (shrink oi)

shrinkOperatorMap :: OperatorMap
                  -> [OperatorMap]
shrinkOperatorMap (OperatorMap m) =
  fmap (OperatorMap . M.fromList) .
  filter (not . null) .
  shrinkList shrinkOperatorEntry .
  M.toList $
  m

instance Arbitrary OperatorMap where
  arbitrary =
    genOperatorMap
  shrink =
    shrinkOperatorMap

data Expr =
    Const Int
  | Op String Expr Expr
  deriving (Eq, Show)

genExpr :: OperatorMap
        -> Gen Expr
genExpr =
  sized .
  genExpr'

genExpr' :: OperatorMap
         -> Int
         -> Gen Expr
genExpr' _ 0 =
    Const <$> arbitrary
genExpr' om@(OperatorMap m) s =
    oneof [
      Const <$> arbitrary
    , Op <$> elements (M.keys m) <*> child2 <*> child2
    ]
  where
    s2 = s `div` 2
    child2 = genExpr' om s2

shrinkExpr :: Expr
           -> [Expr]
shrinkExpr (Const _) =
  []
shrinkExpr (Op s e1 e2) =
  e1 :
  e2 :
  fmap (\e1' -> Op s e1' e2) (shrinkExpr e1) ++
  fmap (\e2' -> Op s e1 e2') (shrinkExpr e2)

toPrettyRules :: OperatorMap
              -> [PrettyRule Expr]
toPrettyRules (OperatorMap m) =
    intRule : map convertEntry (M.toList m)
  where
    intRule =
      PrettyRegular prettyInt
    prettyInt (Const i) =
      Just $ int i
    prettyInt _ =
      Nothing
    convertEntry (s, oi) =
      PrettyOp oi (matchOp s) (prettyOp s)
    matchOp s (Op t e1 e2)
      | s == t = Just (e1, e2)
      | otherwise = Nothing
    matchOp _ _ =
      Nothing
    prettyOp s d1 d2 =
      d1 <+> text s <+> d2

mkExprPretty :: OperatorMap
             -> Expr
             -> Doc
mkExprPretty m =
  mkPretty (toPrettyRules m)

toParserRules :: TokenParsing m
              => OperatorMap
              -> [ParseRule m Expr]
toParserRules (OperatorMap m) =
    intRule : map convertEntry (M.toList m)
  where
    intRule =
      ParseRegular parseInt
    parseInt =
      (Const . fromInteger) <$> integer
    convertEntry (s, oi) =
      ParseOp oi ((\s' -> Op s') <$> symbol s)

mkExprParser :: (TokenParsing m)
             => OperatorMap
             -> m Expr
mkExprParser m =
  mkParser (toParserRules m)

propPrettyParse :: OperatorMap
                -> Property
propPrettyParse om =
  let
    parseExpr = mkExprParser om
    prettyExpr = mkExprPretty om
    roundTrip =
      parseFromString parseExpr .
      prettyToString .
      prettyExpr
  in
    forAllShrink (genExpr om) shrinkExpr $ \expr ->
      case roundTrip expr of
        Left _ -> property False
        Right expr' -> expr === expr'

findParens :: String
           -> [(Int, Int)]
findParens =
    (\(x,_,_) -> x) . foldr f ([], [], 0 :: Int) . reverse
  where
    f '(' (done, opens, ix) =
      (done, ix : opens, ix + 1)
    f ')' (done, o : opens, ix) =
      ((o, ix) : done, opens, ix + 1)
    f _ (done, opens, ix) =
      (done, opens, ix + 1)

removeParens :: String
             -> (Int, Int)
             -> String
removeParens str (s, e) =
  map fst .
  filter ((\i -> i /= s && i /= e) . snd) .
  zip str $
  [0..]

-- if there are no parens, this will return an empty list
-- this is actually what we want
parenRemovals :: String
              -> [String]
parenRemovals s =
  map (removeParens s) .
  findParens $
  s

propPrettyParseParens :: OperatorMap
                      -> Property
propPrettyParseParens om =
  let
    parseExpr = mkExprParser om
    prettyExpr = mkExprPretty om
    roundTripsWithParensRemoved =
      fmap (parseFromString parseExpr) .
      parenRemovals .
      prettyToString .
      prettyExpr
  in
    forAllShrink (genExpr om) shrinkExpr $ \expr ->
      flip all (roundTripsWithParensRemoved expr) $ \rt ->
        case rt of
          Left _ ->
            True
          Right expr' ->
            expr /= expr'
