{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.TS.GenericTests (
    genericTests
) where

import Data.Aeson
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS
import Servant.TS.TestHelpers

data X = X Int Bool Text
    deriving (Show, Generic, Typeable, ToJSON, TsTypeable)

data Union = UnionLeft Int
           | UnionMiddle
           | UnionRight Text
    deriving (Show, Generic, Typeable, ToJSON, TsTypeable)

genericTests :: TestTree
genericTests = testGroup "Aeson <-> TS generic deriving isomorphic"
    [ makeTest (X 0 False "a")
    , makeTest (UnionLeft 7)
    , makeTest UnionMiddle
    , makeTest (UnionRight "a")
    ]