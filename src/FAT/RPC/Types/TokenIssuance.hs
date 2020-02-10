
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Issuance where

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

data Issuance = Issuance {
    issuanceSupply :: Int,
    issuanceSymbol :: Text,
    issuanceType   :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Issuance where
  parseJSON (Object v) =
    Issuance <$> v .: "supply" <*> v .: "symbol" <*> v .: "type"
  parseJSON _ = mzero


instance ToJSON Issuance where
  toJSON (Issuance {..}) = object
    [ "supply" .= issuanceSupply
    , "symbol" .= issuanceSymbol
    , "type" .= issuanceType
    ]
  toEncoding (Issuance {..}) = pairs
    (  "supply"
    .= issuanceSupply
    <> "symbol"
    .= issuanceSymbol
    <> "type"
    .= issuanceType
    )


data TopLevel = TopLevel {
    topLevelChainid   :: Text,
    topLevelTokenid   :: Text,
    topLevelEntryhash :: Text,
    topLevelIssuance  :: Issuance,
    topLevelTimestamp :: Int,
    topLevelIssuerid  :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel
      <$> v
      .:  "chainid"
      <*> v
      .:  "tokenid"
      <*> v
      .:  "entryhash"
      <*> v
      .:  "issuance"
      <*> v
      .:  "timestamp"
      <*> v
      .:  "issuerid"
  parseJSON _ = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object
    [ "chainid" .= topLevelChainid
    , "tokenid" .= topLevelTokenid
    , "entryhash" .= topLevelEntryhash
    , "issuance" .= topLevelIssuance
    , "timestamp" .= topLevelTimestamp
    , "issuerid" .= topLevelIssuerid
    ]
  toEncoding (TopLevel {..}) = pairs
    (  "chainid"
    .= topLevelChainid
    <> "tokenid"
    .= topLevelTokenid
    <> "entryhash"
    .= topLevelEntryhash
    <> "issuance"
    .= topLevelIssuance
    <> "timestamp"
    .= topLevelTimestamp
    <> "issuerid"
    .= topLevelIssuerid
    )
