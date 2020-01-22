{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

data TestUnion = UnionA Int Bool Text
               | UnionB { fieldA :: Text, fieldB :: Int }
               | UnionC
               
data A a = A a
deriveTsTypeable defaultOptions ''A

data B b = B b (A b)
deriveTsTypeable defaultOptions ''B

               {- deriving (Generic, Typeable, TsStrategy 'TsGeneric, TsTypeable) -}
deriveTsJSON defaultOptions{fieldLabelModifier = drop 2} ''TestUnion

type SampleApi = "union" :> Get '[JSON] TestUnion
            :<|> "partB" :> Get '[JSON] ((B Int), (B Text))

main :: IO ()
main = TIO.putStrLn (tsForAPI (Proxy :: Proxy SampleApi) defaultTsGenOptions)