
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Transaction where

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

data Inputs = Inputs {
    inputsFA1zT4aFpEvcnPqPCigB3fvGu4Q4mTXY22iiuV69DqE1pNhdF2MC :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Inputs where
  parseJSON (Object v) =
    Inputs <$> v .: "FA1zT4aFpEvcnPqPCigB3fvGu4Q4mTXY22iiuV69DqE1pNhdF2MC"
  parseJSON _ = mzero


instance ToJSON Inputs where
  toJSON (Inputs {..}) = object
    [ "FA1zT4aFpEvcnPqPCigB3fvGu4Q4mTXY22iiuV69DqE1pNhdF2MC"
        .= inputsFA1zT4aFpEvcnPqPCigB3fvGu4Q4mTXY22iiuV69DqE1pNhdF2MC
    ]
  toEncoding (Inputs {..}) = pairs
    (  "FA1zT4aFpEvcnPqPCigB3fvGu4Q4mTXY22iiuV69DqE1pNhdF2MC"
    .= inputsFA1zT4aFpEvcnPqPCigB3fvGu4Q4mTXY22iiuV69DqE1pNhdF2MC
    )


data Outputs = Outputs {
    outputsFA3aECpw3gEZ7CMQvRNxEtKBGKAos3922oqYLcHQ9NqXHudC6YBM :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Outputs where
  parseJSON (Object v) =
    Outputs <$> v .: "FA3aECpw3gEZ7CMQvRNxEtKBGKAos3922oqYLcHQ9NqXHudC6YBM"
  parseJSON _ = mzero


instance ToJSON Outputs where
  toJSON (Outputs {..}) = object
    [ "FA3aECpw3gEZ7CMQvRNxEtKBGKAos3922oqYLcHQ9NqXHudC6YBM"
        .= outputsFA3aECpw3gEZ7CMQvRNxEtKBGKAos3922oqYLcHQ9NqXHudC6YBM
    ]
  toEncoding (Outputs {..}) = pairs
    (  "FA3aECpw3gEZ7CMQvRNxEtKBGKAos3922oqYLcHQ9NqXHudC6YBM"
    .= outputsFA3aECpw3gEZ7CMQvRNxEtKBGKAos3922oqYLcHQ9NqXHudC6YBM
    )


data Data = Data {
    dataInputs  :: Inputs,
    dataOutputs :: Outputs
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Data where
  parseJSON (Object v) = Data <$> v .: "inputs" <*> v .: "outputs"
  parseJSON _          = mzero


instance ToJSON Data where
  toJSON (Data {..}) =
    object ["inputs" .= dataInputs, "outputs" .= dataOutputs]
  toEncoding (Data {..}) =
    pairs ("inputs" .= dataInputs <> "outputs" .= dataOutputs)


data TopLevel = TopLevel {
    topLevelData      :: Data,
    topLevelEntryhash :: Text,
    topLevelTimestamp :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel <$> v .: "data" <*> v .: "entryhash" <*> v .: "timestamp"
  parseJSON _ = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object
    [ "data" .= topLevelData
    , "entryhash" .= topLevelEntryhash
    , "timestamp" .= topLevelTimestamp
    ]
  toEncoding (TopLevel {..}) = pairs
    (  "data"
    .= topLevelData
    <> "entryhash"
    .= topLevelEntryhash
    <> "timestamp"
    .= topLevelTimestamp
    )
