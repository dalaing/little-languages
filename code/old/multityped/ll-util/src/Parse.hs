module Parse where

import Control.Monad.Except

import qualified Text.Trifecta as T
import Text.PrettyPrint.ANSI.Leijen (Doc)

parseString :: MonadError Doc m => T.Parser r -> String -> m r
parseString p s = case T.parseString p (T.Lines 0 0 0 0) s of
  T.Success r -> return r
  T.Failure d -> throwError d
