
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module NFTokens where

import           Control.Applicative
import           Control.Monad                   (forM_, join, mzero)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..), eitherDecode,
                                                  object, pairs, (.:), (.:?),
                                                  (.=))
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified GHC.Generics
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import           System.IO                       (hPutStrLn, stderr)

--------------------------------------------------------------------------------

data TopLevelElt = TopLevelElt {
    topLevelEltOwner :: Text,
    topLevelEltId    :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevelElt where
  parseJSON (Object v) = TopLevelElt <$> v .: "owner" <*> v .: "id"
  parseJSON _          = mzero


instance ToJSON TopLevelElt where
  toJSON (TopLevelElt {..}) =
    object ["owner" .= topLevelEltOwner, "id" .= topLevelEltId]
  toEncoding (TopLevelElt {..}) =
    pairs ("owner" .= topLevelEltOwner <> "id" .= topLevelEltId)


type TopLevel = [TopLevelElt]
