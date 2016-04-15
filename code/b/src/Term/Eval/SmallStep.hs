{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Small-step rules and helpers for the B language.
-}
module Term.Eval.SmallStep (
    smallStepRules
  , smallStep
  , eval
  , isNormalForm
  , canStep
  ) where

import           Data.Foldable (asum)
import           Data.Maybe    (isJust, isNothing)

import           Term          (Term (..))

-- |
eIfTrue :: Term        -- ^
        -> Maybe Term  -- ^
eIfTrue (TmIf TmTrue t2 _) =
  Just t2
eIfTrue _ =
  Nothing

-- |
eIfFalse :: Term        -- ^
         -> Maybe Term  -- ^
eIfFalse (TmIf TmFalse _ t3) =
  Just t3
eIfFalse _ =
  Nothing

-- |
eIf :: (Term -> Maybe Term) -- ^
    -> Term                 -- ^
    -> Maybe Term           -- ^
eIf step (TmIf tm1 tm2 tm3) = do
  tm1' <- step tm1
  return $ TmIf tm1' tm2 tm3
eIf _ _ =
  Nothing

-- |
smallStepRules :: [Term -> Maybe Term] -- ^
smallStepRules =
  [ eIfTrue
  , eIfFalse
  , eIf smallStep
  ]

-- |
smallStep :: Term       -- ^
          -> Maybe Term -- ^
smallStep tm =
  asum .
  fmap ($ tm) $
  smallStepRules

-- |
eval :: Term -- ^
     -> Term -- ^
eval tm =
  case smallStep tm of
    Nothing -> tm
    Just tm' -> eval tm'

-- |
isNormalForm :: Term -- ^
             -> Bool -- ^
isNormalForm =
  isNothing .
  smallStep

-- |
canStep :: Term -- ^
        -> Bool -- ^
canStep =
  isJust .
  smallStep
