
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module GetBalances where

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
    topLevel_0cccd100a1801c0cf4aa2104b15dec94fe6f45d0f3347b016ed20d81059494df :: Int,
    topLevel_962a18328c83f370113ff212bae21aaf34e5252bc33d59c9db3df2a6bfda966f :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel
      <$> v
      .:  "0cccd100a1801c0cf4aa2104b15dec94fe6f45d0f3347b016ed20d81059494df"
      <*> v
      .:  "962a18328c83f370113ff212bae21aaf34e5252bc33d59c9db3df2a6bfda966f"
  parseJSON _ = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object
    [ "0cccd100a1801c0cf4aa2104b15dec94fe6f45d0f3347b016ed20d81059494df"
      .= topLevel_0cccd100a1801c0cf4aa2104b15dec94fe6f45d0f3347b016ed20d81059494df
    , "962a18328c83f370113ff212bae21aaf34e5252bc33d59c9db3df2a6bfda966f"
      .= topLevel_962a18328c83f370113ff212bae21aaf34e5252bc33d59c9db3df2a6bfda966f
    ]
  toEncoding (TopLevel {..}) = pairs
    (  "0cccd100a1801c0cf4aa2104b15dec94fe6f45d0f3347b016ed20d81059494df"
    .= topLevel_0cccd100a1801c0cf4aa2104b15dec94fe6f45d0f3347b016ed20d81059494df
    <> "962a18328c83f370113ff212bae21aaf34e5252bc33d59c9db3df2a6bfda966f"
    .= topLevel_962a18328c83f370113ff212bae21aaf34e5252bc33d59c9db3df2a6bfda966f
    )
