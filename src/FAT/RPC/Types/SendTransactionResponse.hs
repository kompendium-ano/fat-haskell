
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module SendTransactionResponse where

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

data TopLevel = TopLevel {
    topLevelChainid   :: Text,
    topLevelEntryhash :: Text,
    topLevelTxid      :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel <$> v .: "chainid" <*> v .: "entryhash" <*> v .: "txid"
  parseJSON _ = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object
    [ "chainid" .= topLevelChainid
    , "entryhash" .= topLevelEntryhash
    , "txid" .= topLevelTxid
    ]
  toEncoding (TopLevel {..}) = pairs
    (  "chainid"
    .= topLevelChainid
    <> "entryhash"
    .= topLevelEntryhash
    <> "txid"
    .= topLevelTxid
    )
