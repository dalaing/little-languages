{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Bool.Gen (
    genTermInput
  ) where

import           Control.Lens         (preview, review)
import           Test.QuickCheck      (Gen)

import Component.Term.Gen (GenAnyTermRule(..), ShrAnyTermRule(..), GenTermInput(..), GenValueTermRule(..), ShrValueTermRule(..), GenNonValueTermRule(..), ShrNonValueTermRule(..))

import           Component.Term.Bool (AsBoolTerm (..), WithBoolTerm)

-- |
genTmFalse :: WithBoolTerm tm n a
           => Gen (tm n a)
genTmFalse =
  pure $ review _TmFalse ()

-- |
shrinkTmFalse :: WithBoolTerm tm n a
              => tm n a        -- ^
              -> Maybe [tm n a] -- ^
shrinkTmFalse =
  fmap (const []) .
  preview _TmFalse

-- |
genTmTrue :: WithBoolTerm tm n a
          => Gen (tm n a)
genTmTrue =
  pure $ review _TmTrue ()

-- |
shrinkTmTrue :: WithBoolTerm tm n a
             => tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmTrue =
  fmap (const []) .
  preview _TmTrue

-- |
genTmIf :: WithBoolTerm tm n a
        => Gen (tm n a) -- ^
        -> Gen (tm n a) -- ^
        -> Gen (tm n a) -- ^
        -> Gen (tm n a) -- ^
genTmIf g1 g2 g3 =
  fmap (review _TmIf)
    ((,,) <$> g1 <*> g2 <*> g3)

-- |
shrinkTmIf :: WithBoolTerm tm n a
           => (tm n a -> [tm n a]) -- ^
           -> (tm n a -> [tm n a]) -- ^
           -> (tm n a -> [tm n a]) -- ^
           -> tm n a          -- ^
           -> Maybe [tm n a]   -- ^
shrinkTmIf s1 s2 s3 =
    fmap shrinkTmIf' .
    preview _TmIf
  where
    shrinkTmIf' (tm1, tm2, tm3) =
      s1 tm1 ++ [tm1] ++
      s2 tm2 ++ [tm2] ++
      s3 tm3 ++ [tm3] ++
      fmap (\tm1' -> review _TmIf (tm1', tm2, tm3)) (s1 tm1) ++
      fmap (\tm2' -> review _TmIf (tm1, tm2', tm3)) (s2 tm2) ++
      fmap (\tm3' -> review _TmIf (tm1, tm2, tm3')) (s3 tm3)

genValueTmFalse :: WithBoolTerm tm n a
                => Gen (tm n a)
genValueTmFalse =
  genTmFalse

shrValueTmFalse :: WithBoolTerm tm n a
                => tm n a
                -> Maybe [tm n a]
shrValueTmFalse =
  shrinkTmFalse

genValueTmTrue :: WithBoolTerm tm n a
               => Gen (tm n a)
genValueTmTrue =
  genTmTrue

shrValueTmTrue :: WithBoolTerm tm n a
               => tm n a
               -> Maybe [tm n a]
shrValueTmTrue =
  shrinkTmTrue

genNonValueTmIf :: WithBoolTerm tm n a
                  => (Int -> Gen (tm n a))
                  -> Int
                  -> Gen (tm n a)
genNonValueTmIf g s =
  let
    g' = g (s `div` 3)
  in
    genTmIf g' g' g'

shrNonValueTmIf :: WithBoolTerm tm n a
                => (tm n a -> [tm n a])
                -> tm n a
                -> Maybe [tm n a]
shrNonValueTmIf s =
    fmap shrNonValueTmIf' .
    preview _TmIf
  where
    shrNonValueTmIf' (tm1, tm2, tm3) =
      fmap (\tm1' -> review _TmIf (tm1', tm2, tm3)) (s tm1) ++
      fmap (\tm2' -> review _TmIf (tm1, tm2', tm3)) (s tm2) ++
      fmap (\tm3' -> review _TmIf (tm1, tm2, tm3')) (s tm3)

genTermInput :: WithBoolTerm tm n a
             => GenTermInput tm n a
genTermInput =
  GenTermInput
    [ GenAnyTermBase genTmFalse
    , GenAnyTermBase genTmTrue
    , GenAnyTermRecurse $ \g s ->
        let
          child = g (s `div` 3)
        in
          genTmIf child child child
    ]
    [ ShrAnyTermBase shrinkTmFalse
    , ShrAnyTermBase shrinkTmTrue
    , ShrAnyTermRecurse $ \s -> shrinkTmIf s s s
    ]
    [ GenValueTermBase genValueTmFalse
    , GenValueTermBase genValueTmTrue
    ]
    [ ShrValueTermBase shrValueTmFalse
    , ShrValueTermBase shrValueTmTrue
    ]
    [ GenNonValueTermRecurse genNonValueTmIf
    ]
    [ ShrNonValueTermRecurse shrNonValueTmIf
    ]
