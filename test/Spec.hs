{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS

import Servant.TS.InstanceTests

main = defaultMain tests

data X = X Int Bool Text
    deriving (Generic, Typeable, ToJSON, TsTypeable)

derivingTests :: TestTree
derivingTests = testGroup "Aeson <-> TS deriving isomorphic" 
    [ {- testCase "Basic unnamed type" $ makeTest (X 0 False "a") -}]

tests :: TestTree
tests = testGroup "Aeson <-> TS" 
    [ instanceTests, derivingTests ]