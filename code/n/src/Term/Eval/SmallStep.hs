{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Small-step rules and helpers for the N language.

This module reexports 'Term.Eval.SmallStep.Strict'.
There is also 'Term.Eval.SmallStep.Lazy' if you need it.
-}
module Term.Eval.SmallStep (
    module Term.Eval.SmallStep.Strict
  ) where

-- local
import Term.Eval.SmallStep.Strict
