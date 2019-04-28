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

import Servant.TS.GenericTests
import Servant.TS.InstanceTests
import Servant.TS.TestHelperTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Aeson <-> TS" 
    [ validationTests, instanceTests, genericTests ]