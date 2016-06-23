module Rules.Json where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Control.Monad

import Data.Profunctor

import Rules.Div

data Coord = Coord Int Int

{-
instance ToJSON Coord where
 toJson (Coord x y) = object ["x" .= x, y .= "y"]

instance FromJSON Coord where
  fromJson (Object o) = Coord <$> o .: "x" <*> o .: "y"
  fromJson _ = empty
-}

kv :: (ToJSON a, FromJSON b) => Text -> JsonThing Object a b
kv t = JsonThing (Just . object . (: []) . (t .= )) (.: t)

-- a -> Pair   and Object -> Parser a1

kvX :: JsonThing Object Int Int
kvX = kv (pack "x")

kvY :: JsonThing Object Int Int
kvY = kv (pack "y")

kvXY :: JsonThing Object (Int, Int) (Int, Int)
kvXY = divide1 id kvX kvY

-- kv "x" # kv "y"

-- JsonThing Value Object Object
-- Text -> JsonThing Object Pair Pair
-- JsonThing Pair Value Value

bam :: JsonThing a b b -> JsonThing b c c -> JsonThing a c c
bam (JsonThing g1 p1) (JsonThing g2 p2) = JsonThing (g2 >=> g1) (p1 >=> p2)

data JsonThing t a b = JsonThing (a -> Maybe t) (t -> Parser b)

instance Profunctor (JsonThing t) where
    dimap l r (JsonThing x y) = JsonThing (x . l) (fmap (fmap r) y)

instance Functor (JsonThing t a) where
    fmap = rmap

instance Contravariant1 (JsonThing t) where
    contramap1 = lmap

instance Divide1 (JsonThing t) where
  divide1 split (JsonThing x1 y1) (JsonThing x2 y2) = 
    JsonThing (\a -> case split a of (b, c) -> _ (x1 b) (x2 c)) ((,) <$> y1 <*> y2)
{-

instance Applicative (JsonThing a) where
    pure x = JsonThing (const Nothing) (pure x) 
-}
