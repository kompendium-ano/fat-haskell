
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module DaemonTokens where

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
    topLevelEltChainid  :: Text,
    topLevelEltTokenid  :: Text,
    topLevelEltIssuerid :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevelElt where
  parseJSON (Object v) =
    TopLevelElt <$> v .: "chainid" <*> v .: "tokenid" <*> v .: "issuerid"
  parseJSON _ = mzero


instance ToJSON TopLevelElt where
  toJSON (TopLevelElt {..}) = object
    [ "chainid" .= topLevelEltChainid
    , "tokenid" .= topLevelEltTokenid
    , "issuerid" .= topLevelEltIssuerid
    ]
  toEncoding (TopLevelElt {..}) = pairs
    (  "chainid"
    .= topLevelEltChainid
    <> "tokenid"
    .= topLevelEltTokenid
    <> "issuerid"
    .= topLevelEltIssuerid
    )


type TopLevel = [TopLevelElt]
