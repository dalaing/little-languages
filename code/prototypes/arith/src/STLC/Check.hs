module STLC.Check where

import Data.List.NonEmpty
import Data.Validation

import Control.Lens

import STLC
import STLC.Type

data Error e = Stuck | Expected e e | Arg Int (Error e)
type Errors e = NonEmpty (Error e)

expect :: Type -> AccValidation (Errors Type) Type -> AccValidation (Errors Type) Type
expect ex = over _Validation expectV
  where
    expectV :: Validation (Errors Type) Type -> Validation (Errors Type) Type
    expectV v = v >>= \ac -> if ex == ac then _Success # ex else _Failure # (Expected ex ac :| [])

expectArg :: Int -> Type -> AccValidation (Errors Type) Type -> AccValidation (Errors Type) Type
expectArg n ex = over _Validation expectArgV
  where
    expectArgV :: Validation (Errors Type) Type -> Validation (Errors Type) Type
    expectArgV v = v >>= \ac -> if ex == ac then _Success # ex else _Failure # (Arg n (Expected ex ac) :| [])

tcLoc :: (Term l n a -> AccValidation (Errors Type) Type) -> Term l n a -> Maybe (AccValidation (Errors Type) Type)
tcLoc step = fmap step . preview (_TmLoc . _2)

tcInt :: Term l n a -> Maybe (AccValidation (Errors Type) Type)
tcInt = fmap (const $ _Success # TyInt) . preview _TmInt

tcBool :: Term l n a -> Maybe (AccValidation (Errors Type) Type)
tcBool = fmap (const $ _Success # TyBool) . preview _TmBool

tcAdd :: (Term l n a -> AccValidation (Errors Type) Type) -> Term l n a -> Maybe (AccValidation (Errors Type) Type)
tcAdd step =
    fmap (\(x,y) ->
           (_Success # (\_ _ -> TyInt)) <*> expectArg 0 TyInt (step x) <*> expectArg 1 TyInt (step y)
         ) .
    preview _TmAdd

tcEq :: (Term l n a -> AccValidation (Errors Type) Type) -> Term l n a -> Maybe (AccValidation (Errors Type) Type)
tcEq step =
    fmap (\(x,y) ->
           (_Success # (\_ _ -> TyBool)) <*> expectArg 0 TyInt (step x) <*> expectArg 1 TyInt (step y)
         ) .
    preview _TmEq

tcAnd :: (Term l n a -> AccValidation (Errors Type) Type) -> Term l n a -> Maybe (AccValidation (Errors Type) Type)
tcAnd step =
    fmap (\(x,y) ->
           (_Success # (\_ _ -> TyBool)) <*> expectArg 0 TyBool (step x) <*> expectArg 1 TyBool (step y)
         ) .
    preview _TmAnd
