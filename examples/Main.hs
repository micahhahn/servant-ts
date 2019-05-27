{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Typeable
import GHC.Generics
import Servant.API

import Servant.TS
import Servant.TS.TH

type SampleApi = "union" :> Get '[JSON] TestUnion

data TestUnion = UnionA Int Bool Text
               | UnionB { fieldA :: Text, fieldB :: Int }
               | UnionC
               
               {- deriving (Generic, Typeable, TsStrategy 'TsGeneric, TsTypeable) -}
deriveTsJSON defaultOptions{fieldLabelModifier = drop 2} ''TestUnion

main :: IO ()
main = TIO.putStrLn (tsForAPI (Proxy :: Proxy SampleApi) defaultTsGenOptions)
    