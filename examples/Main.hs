{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Typeable
import GHC.Generics
import Servant.API

import Servant.TS

type SampleApi = "union" :> Get '[JSON] TestUnion

data TestUnion = UnionA Int
               | UnionB { fieldA :: Text, fieldB :: Int}
               deriving (Generic, Typeable, TsTypeable)

main :: IO ()
main = TIO.putStrLn (tsForAPI (Proxy :: Proxy SampleApi) defaultTsGenOptions)
    