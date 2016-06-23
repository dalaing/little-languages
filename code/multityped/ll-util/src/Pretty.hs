module Pretty where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""
