{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Value rules and helpers for the NB language.

This module reexports 'Term.Eval.Value.Strict'.
There is also 'Term.Eval.Value.Lazy' if you need it.
-}
module Term.Eval.Value (
    module Term.Eval.Value.Strict
  ) where

-- local
import Term.Eval.Value.Strict
