{-# LANGUAGE ScopedTypeVariables #-}
module Common.Recursion where

import Control.Applicative (Alternative)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Control.Monad.Reader (ReaderT(..))

data Step a =
    SBase a
  | SRecurse (a -> a)

apStep :: a
       -> Step a
       -> a
apStep _    (SBase f)    = f
apStep step (SRecurse f) = f step

mkSteps :: Alternative f
        => [Step (f b)]
        -> [f b]
mkSteps ss =
  let
    step = combineSteps ss
    f = apStep step
  in
    fmap f ss

combineSteps :: Alternative f
             => [Step (f b)]
             -> f b
combineSteps ss =
  let
    step =
      asum .
      fmap (apStep step) $
      ss
  in
    step

stepFnToReader :: Step (a -> m b)
               -> Step (ReaderT a m b)
stepFnToReader (SBase f) =
  SBase (ReaderT f)
stepFnToReader (SRecurse f) =
  SRecurse $ ReaderT . f . runReaderT

data MaybeStep a b =
    MSBase (a -> Maybe b)
  | MSRecurse ((a -> b) -> a -> Maybe b)

apMaybeStep :: (a -> b)
            -> a
            -> MaybeStep a b
            -> Maybe b
apMaybeStep _    x (MSBase f)    = f x
apMaybeStep step x (MSRecurse f) = f step x

mkMaybeSteps :: b
             -> [MaybeStep a b]
             -> [a -> Maybe b]
mkMaybeSteps d ss =
  let
    step = combineMaybeSteps d ss
    f s x = apMaybeStep step x s
  in
    fmap f ss

combineMaybeSteps :: b
                  -> [MaybeStep a b]
                  -> a
                  -> b
combineMaybeSteps d ss =
  let
    step x =
      fromMaybe d .
      asum .
      fmap (apMaybeStep step x) $
      ss
  in
    step
