{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for terms of the B language.
-}
module Term.Gen (
    genTerm
  , shrinkTerm
  ) where

import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe)
import           Test.QuickCheck (Gen, oneof, sized)

import           Term            (Term (..))

-- |
genTmFalse :: Gen Term -- ^
genTmFalse =
  pure TmFalse

-- |
shrinkTmFalse :: Term         -- ^
              -> Maybe [Term] -- ^
shrinkTmFalse TmFalse =
  Just []
shrinkTmFalse _ =
  Nothing

-- |
genTmTrue :: Gen Term -- ^
genTmTrue =
  pure TmTrue

-- |
shrinkTmTrue :: Term         -- ^
             -> Maybe [Term] -- ^
shrinkTmTrue TmTrue =
  Just []
shrinkTmTrue _ =
  Nothing

-- |
genTmIf :: Gen Term -- ^
        -> Gen Term -- ^
        -> Gen Term -- ^
        -> Gen Term -- ^
genTmIf g1 g2 g3 =
  TmIf <$> g1 <*> g2 <*> g3

-- |
shrinkTmIf :: (Term -> [Term]) -- ^
           -> (Term -> [Term]) -- ^
           -> (Term -> [Term]) -- ^
           -> Term             -- ^
           -> Maybe [Term]     -- ^
shrinkTmIf s1 s2 s3 (TmIf tm1 tm2 tm3) = Just $
  s1 tm1 ++ [tm1] ++
  s2 tm2 ++ [tm2] ++
  s3 tm3 ++ [tm3] ++
  fmap (\tm1' -> TmIf tm1' tm2 tm3) (s1 tm1) ++
  fmap (\tm2' -> TmIf tm1 tm2' tm3) (s2 tm2) ++
  fmap (\tm3' -> TmIf tm1 tm2 tm3') (s3 tm3)
shrinkTmIf _ _ _ _ =
  Nothing

-- |
genTerm :: Gen Term -- ^
genTerm = sized genTerm'

-- |
genTerm' :: Int      -- ^
         -> Gen Term -- ^
genTerm' 0 =
  oneof
    [ genTmFalse
    , genTmTrue
    ]
genTerm' s =
    oneof
      [ genTmFalse
      , genTmTrue
      , genTmIf child3 child3 child3
      ]
  where
    s3 = s `div` 3
    child3 = genTerm' s3

-- |
shrinkTerm :: Term   -- ^
           -> [Term] -- ^
shrinkTerm tm =
  fromMaybe [] .
  asum .
  fmap ($ tm) $
    [ shrinkTmFalse
    , shrinkTmTrue
    , shrinkTmIf shrinkTerm shrinkTerm shrinkTerm
    ]
