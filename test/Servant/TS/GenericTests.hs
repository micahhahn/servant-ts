{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Servant.TS.GenericTests (
    genericTests
) where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS
import Servant.TS.TestHelpers

data X = X Int Bool Text
    deriving (Show, Generic, Typeable, ToJSON, TsStrategy 'TsGeneric, TsTypeable)

data Union = UnionLeft Int
           | UnionMiddle
           | UnionRight Text
    deriving (Show, Generic, Typeable, ToJSON, TsStrategy 'TsGeneric, TsTypeable)

data RecordUnion = RUnionLeft { a :: Int }
                 | RUnionRight { b :: Text, c :: Bool }
                 deriving (Show, Generic, Typeable, ToJSON, TsStrategy 'TsGeneric, TsTypeable)

data BadUnion = BUnionLeft { tag :: Int, d :: Text }
              | BUnionRight Text
              deriving (Show, Generic, Typeable, ToJSON, TsStrategy 'TsGeneric, TsTypeable)

data Poly a = Poly a
            deriving (Show, Generic, Generic1, Typeable, ToJSON, TsStrategy 'TsGeneric1, TsTypeable)

genericTests :: TestTree
genericTests = testGroup "Aeson <-> TS generic deriving isomorphic"
    [ makeTest (X 0 False "a")

    , makeTest (UnionLeft 7)
    , makeTest UnionMiddle
    , makeTest (UnionRight "a")

    , makeTest (RUnionLeft 7)
    , makeTest (RUnionRight "a" False)

    , makeTest (BUnionLeft 7 "a")
    , makeTest (BUnionRight "b")

    , makeTest (Poly 7 :: Poly Int)
    ]