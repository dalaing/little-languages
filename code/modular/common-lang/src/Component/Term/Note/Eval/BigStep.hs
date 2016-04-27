{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Note.Eval.BigStep (
    bigStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.BigStep (BigStepRule(..), BigStepInput(..))
import Component.Term.Note (AsNoteTerm(..), WithNoteTerm)

bigStepTmNote :: WithNoteTerm tm
                => (tm n a -> Maybe (tm n a))
                -> tm n a
                -> Maybe (tm n a)
bigStepTmNote bigStep tm = do
  (n, tm1) <- preview _TmNote tm
  tm1' <- bigStep tm1
  return $ review _TmNote (n, tm1')

bigStepInput :: WithNoteTerm tm
           => BigStepInput tm n a
bigStepInput =
  BigStepInput
    [BigStepRecurse bigStepTmNote]
