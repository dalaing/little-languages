module Term.Value where

class IsValue t where
  isValue :: t -> Bool
