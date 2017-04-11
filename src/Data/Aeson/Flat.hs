{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Aeson.Flat where

-- import Debug.Trace (trace)
import Control.Applicative ((<|>))
import Data.Aeson (Value(..))
import Data.Aeson.Types (genericToJSON, genericParseJSON, defaultOptions, GToJSON, GFromJSON, Parser, typeMismatch, Zero)
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic, Rep)
-- import qualified Data.ByteString.Lazy.Char8 as BSL



-- | Merge values together. Useful for creating compound JSON structures that should be parsed as one object
--
-- > data A = A { one :: String, two :: String } deriving (Show, Eq, Generic)
-- > instance ToJSON A
-- > instance FromJSON A
-- >
-- > data B = B { three :: String } deriving (Show, Eq, Generic)
-- > instance ToJSON B
-- > instance FromJSON B
-- >
-- > data AB = AB A B deriving (Show, Eq)
-- >
-- > instance ToJSON AB where
-- >   toJSON (AB a b) = merge [toJSON a, toJSON b]
-- >
-- > instance FromJSON AB where
-- >   parseJSON o = do
-- >     a <- parseJSON o
-- >     b <- parseJSON o
-- >     return $ AB a b

merge :: [Value] -> Value
merge [] = Null
merge (v:vs) = foldl append v vs
  where
    append (Object a) (Object b) =
      Object $ a <> b
    append (Array a) (Array b) =
      Array $ a <> b
    append (String a) (String b) =
      String $ a <> b
    append (Number a) (Number b) =
      Number $ a + b
    append (Bool a) (Bool b) =
      Bool $ a && b
    append _ _ = Null


-- | Serialize a sumtype to a flat object, rather than to "tag" and "contents"
--
-- > data C = CA A | CB B deriving (Show, Eq, Generic)
-- >
-- > instance ToJSON C where
-- >   toJSON x = flatToJSON "c" x
--
-- {"c": "CA", "one": "value", "two": "value"}
-- {"c": "CB", "three": "value"}

flatToJSON :: (Generic a, GToJSON Zero (Rep a)) => Text -> a -> Value
flatToJSON n a =
  flatten n $ genericToJSON defaultOptions a
  where
  flatten n' (Object o) =
    let t = sumTag o
        c = sumContents o
    in Object $ HM.insert n' (String t) c
  flatten _ v = v

  sumTag o =
    case HM.lookup "tag" o of
      Just (String t) ->
        t
      _ ->
        ""

  sumContents o =
    case HM.lookup "contents" o of
      Just (Object c) ->
        c
      Just (Array v) ->
        V.foldl allObjects HM.empty v
      _ ->
        HM.empty

  allObjects m (Object o) = HM.union m o
  allObjects m _ = m


-- | Deserialize a sumtype from a flat object
-- >
-- > instance FromJSON C where
-- >   parseJSON x = flatParseJSON "c" x

flatParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Text -> Value -> Parser a
flatParseJSON n (Object o) =
  case HM.lookup n o of
    Just (String t) ->
      let o' = HM.fromList [("tag", String t), ("contents", Object o)]
          u' = HM.fromList [("tag", String t), ("contents", Array V.empty)]
      in parse o' <|> parse u'
    _ -> typeMismatch ("field: " ++ unpack n) (Object o)
  where
    parse o' = genericParseJSON defaultOptions (Object o')
flatParseJSON _ v = typeMismatch "Object" v



fieldToJSON :: (Generic a, GToJSON Zero (Rep a)) => Text -> a -> Value
fieldToJSON n a =
  flatten n $ genericToJSON defaultOptions a
  where
  flatten n' v =
    Object $ HM.fromList [(n', v)]


fieldParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Text -> Value -> Parser a
fieldParseJSON n (Object o) =
  -- construct what it is expecting: tag, and contents
  case HM.lookup n o of
    Just v ->
      genericParseJSON defaultOptions v
    _ -> typeMismatch ("field: " ++ unpack n) (Object o)
fieldParseJSON _ v = typeMismatch "Object" v


-- | Example of how to use the above for a parent record
--
-- > data Example = Example
-- >   { exa :: String
-- >   , exb :: Int
-- >   , exc :: C
-- >   } deriving (Show, Eq, Generic)
-- >
-- > instance ToJSON Example where
-- >   toJSON ex =
-- >     merge
-- >       [ object [ "a" .= toJSON (exa ex)
-- >                , "b" .= toJSON (exb ex)
-- >                ]
-- >       , toJSON (exc ex)
-- >       ]
-- >
-- > instance FromJSON Example where
-- >   parseJSON (Object o) = do
-- >     a <- o .: "a"
-- >     b <- o .: "b"
-- >     c <- parseJSON (Object o)
-- >     return $ Example a b c



-- test :: IO ()
-- test = do
--
--   let a = A { one = "one", two = "two" }
--       b = B { three = "three" }
--       ab = AB a b
--       c' = CB b
--       ex = Example "a" 1 c'
--
--   -- BSL.putStrLn $ Aeson.encode ab
--   -- print $ (Aeson.eitherDecode $ Aeson.encode ab :: Either String AB)
--
--   BSL.putStrLn $ Aeson.encode c'
--   print $ (Aeson.eitherDecode $ Aeson.encode c' :: Either String C)
--
--   BSL.putStrLn $ Aeson.encode ex
--   print $ (Aeson.eitherDecode $ Aeson.encode ex :: Either String Example)
