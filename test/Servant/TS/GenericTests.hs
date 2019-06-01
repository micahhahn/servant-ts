{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Servant.TS.GenericTests (
    genericTests
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS
import Servant.TS.TH
import Servant.TS.TestHelpers

data TFieldTest = CFieldTest1 { fFieldTest1A :: Int, fFieldTest1B :: Int }
                | CFieldTest2 { fFieldTest2A :: Bool, fFIeldTest2B :: Text }
                deriving (Show)
deriveTsJSON defaultOptions ''TFieldTest

data TFieldLabelModifier = CFieldLabelModifier { fFieldLabelModifier :: Int }
                         deriving (Show)
deriveTsJSON defaultOptions{fieldLabelModifier = drop 3} ''TFieldLabelModifier

data TConstructorTagModifier = CTest1 Int
                             | CTest2 Bool
                             deriving (Show)
deriveTsJSON defaultOptions{constructorTagModifier = drop 1} ''TConstructorTagModifier

data TAllNullaryToStringTag = CAllNullaryToStringTag1
                            | CAllNullaryToStringTag2
                            deriving (Show)
deriveTsJSON defaultOptions{allNullaryToStringTag = False} ''TAllNullaryToStringTag

data TAllNullaryToStringTag' = CAllNullaryToStringTag1'
                             | CAllNullaryToStringTag2'
                             deriving (Show)
deriveTsJSON defaultOptions{allNullaryToStringTag = True} ''TAllNullaryToStringTag'

data TOmitNothingFields = COmitNothingFields (Maybe Int)
                        deriving (Show)
deriveTsJSON defaultOptions{omitNothingFields = True} ''TOmitNothingFields

data TTaggedObject = CTaggedObject1
                   | CTaggedObject2 Int
                   deriving (Show)
deriveTsJSON defaultOptions{sumEncoding = TaggedObject { tagFieldName = "ttag", contentsFieldName = "ccontents" } } ''TTaggedObject

data TUntaggedValue = CUntaggedValue1 Int
                    | CUntaggedValue2 { fUntaggedValue2 :: Text }
                    deriving (Show)
deriveTsJSON defaultOptions{sumEncoding = UntaggedValue} ''TUntaggedValue

data TObjectWithSingleField = CObjectWithSingleField1 Int Int
                            | CObjectWithSingleField2 { fField1 :: Text, fField2 :: Text }
                            deriving (Show)
deriveTsJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''TObjectWithSingleField

data TTwoElemArray = CTwoElemArray1 Int
                   | CTwoElemArray2 Text
                   deriving (Show)
deriveTsJSON defaultOptions{sumEncoding = TwoElemArray} ''TTwoElemArray

data TUnwrapUnaryRecords = CUnwrapUnaryRecords { fUnwrapUnaryRecords :: Int }
                         deriving (Show)
deriveTsJSON defaultOptions{unwrapUnaryRecords = True} ''TUnwrapUnaryRecords

data TTagSingleConstructors = CTagSingleConstructors Int
                            deriving (Show)
deriveTsJSON defaultOptions{tagSingleConstructors = True} ''TTagSingleConstructors

data TPolyTest a = CPolyTest a
                 deriving (Show)
deriveTsJSON defaultOptions ''TPolyTest

genericTests :: TestTree
genericTests = testGroup "Aeson <-> TS generic deriving isomorphic"
    [ makeTest (CFieldLabelModifier 1)
    
    , makeTest (CFieldTest1 7 8)
    , makeTest (CFieldTest2 False "a")

    , makeTest (CTest1 1)
    , makeTest (CTest2 False)

    , makeTest CAllNullaryToStringTag1
    , makeTest CAllNullaryToStringTag2

    , makeTest CAllNullaryToStringTag1'
    , makeTest CAllNullaryToStringTag2'

    , makeTest (COmitNothingFields Nothing)
    , makeTest (COmitNothingFields (Just 1))

    , makeTest CTaggedObject1
    , makeTest (CTaggedObject2 1)

    , makeTest (CUntaggedValue1 1)
    , makeTest (CUntaggedValue2 "a")

    , makeTest (CObjectWithSingleField1 1 1)
    , makeTest (CObjectWithSingleField2 "a" "a")

    , makeTest (CTwoElemArray1 1)
    , makeTest (CTwoElemArray2 "a")

    , makeTest (CUnwrapUnaryRecords 1)
    
    , makeTest (CTagSingleConstructors 1)

    , makeTest (CPolyTest (1 :: Int))
    , makeTest (CPolyTest ("a" :: Text))
    ]