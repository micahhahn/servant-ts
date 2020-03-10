{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Servant.TS.GenericTests (
    genericTests
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS
import Servant.TS.Core
import Servant.TS.TH
import Servant.TS.TestHelpers

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

import qualified Data.HashMap.Strict as HM 

data X = X Int Bool

instance TsTypeable X where
    tsTypeRep _ = TsTuple [ts @Int, ts @Bool, ts @X]

ts :: forall a. TsTypeable a => TsType
ts = tsTypeRep (Proxy :: Proxy a)

-- Test basic record field mapping
data TFieldTest = CFieldTest1 { fFieldTest1A :: Int, fFieldTest1B :: Int }
                | CFieldTest2 { fFieldTest2A :: Bool, fFieldTest2B :: Text }
                deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions ''TFieldTest

tFieldTests = makeTests_G_TH defaultOptions 
                             [ CFieldTest1 7 8
                             , CFieldTest2 False "a" 
                             ]

-- Test field label modifier options
data TFieldLabelModifier = CFieldLabelModifier { fFieldLabelModifier :: Int }
                         deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{fieldLabelModifier = drop 3} ''TFieldLabelModifier

tFieldLabelModifier = makeTests_G_TH defaultOptions{fieldLabelModifier = drop 3} 
                                     [ CFieldLabelModifier 1
                                     , CFieldLabelModifier (-1)
                                     ]

-- Test constructor tag modifier
data TConstructorTagModifier = CTest1 Int
                             | CTest2 Bool
                             deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{constructorTagModifier = drop 1} ''TConstructorTagModifier

tConstructorTagModifier = makeTests_G_TH defaultOptions{constructorTagModifier = drop 1}
                                         [ CTest1 1
                                         , CTest2 False
                                         ]

-- Test the allNullaryToStringTag flag
data TAllNullaryToStringTag = CAllNullaryToStringTag1
                            | CAllNullaryToStringTag2
                            deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{allNullaryToStringTag = False} ''TAllNullaryToStringTag

tAllNullaryToStringTag = makeTests_G_TH defaultOptions{allNullaryToStringTag = False}
                                        [ CAllNullaryToStringTag1 
                                        , CAllNullaryToStringTag2
                                        ]

-- Test the allNullaryToStringTag flag
data TAllNullaryToStringTag' = CAllNullaryToStringTag1'
                             | CAllNullaryToStringTag2'
                             deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{allNullaryToStringTag = True} ''TAllNullaryToStringTag'

tAllNullaryToStringTag' = makeTests_G_TH defaultOptions{allNullaryToStringTag = True}
                                         [ CAllNullaryToStringTag1'
                                         , CAllNullaryToStringTag2'
                                         ]

-- Test the `omitNothingFields` flag
data TOmitNothingFields = COmitNothingFields (Maybe Int)
                        deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{omitNothingFields = True} ''TOmitNothingFields

tOmitNothingFields = makeTests_G_TH defaultOptions{omitNothingFields = True}
                                    [ COmitNothingFields Nothing
                                    , COmitNothingFields (Just 1)
                                    ]

-- Test the `sumEncoding = Tagged Object` flag
data TTaggedObject = CTaggedObject1
                   | CTaggedObject2 Int
                   deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{sumEncoding = TaggedObject { tagFieldName = "ttag", contentsFieldName = "ccontents" } } ''TTaggedObject

tTaggedObject = makeTests_G_TH defaultOptions{sumEncoding = TaggedObject { tagFieldName = "ttag", contentsFieldName = "ccontents" } }
                               [ CTaggedObject1
                               , CTaggedObject2 1
                               ]

-- Test the `sumEncoding = UntaggedValue` flag
data TUntaggedValue = CUntaggedValue1 Int
                    | CUntaggedValue2 { fUntaggedValue2 :: Text }
                    deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{sumEncoding = UntaggedValue} ''TUntaggedValue

tUntaggedValue = makeTests_G_TH defaultOptions{sumEncoding = UntaggedValue}
                                [ CUntaggedValue1 1
                                , CUntaggedValue2 "a"
                                ]

-- Test the `sumEncoding = ObjectWithSingleField` flag
data TObjectWithSingleField = CObjectWithSingleField1 Int Int
                            | CObjectWithSingleField2 { fField1 :: Text, fField2 :: Text }
                            deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''TObjectWithSingleField

tObjectWithSingleField = makeTests_G_TH defaultOptions{sumEncoding = ObjectWithSingleField}
                                        [ CObjectWithSingleField1 1 1
                                        , CObjectWithSingleField2 "a" "a"
                                        ]

-- Test the `sumEncoding = TwoElemArray` flag
data TTwoElemArray = CTwoElemArray1 Int
                   | CTwoElemArray2 Text
                   deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{sumEncoding = TwoElemArray} ''TTwoElemArray

tTwoElemArray = makeTests_G_TH defaultOptions{sumEncoding = TwoElemArray}
                               [ CTwoElemArray1 1
                               , CTwoElemArray2 "a"
                               ]

-- Test the `unwrapUnaryRecords = True` flag
data TUnwrapUnaryRecords = CUnwrapUnaryRecords { fUnwrapUnaryRecords :: Int }
                         deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{unwrapUnaryRecords = True} ''TUnwrapUnaryRecords

tUnwrapUnaryRecords = makeTests_G_TH defaultOptions{unwrapUnaryRecords = True}
                                     [ CUnwrapUnaryRecords 1
                                     ]

-- Test the `tagSingleConstructors = True` flag 
data TTagSingleConstructors = CTagSingleConstructors Int
                            deriving (Show, Typeable, Generic)
deriveTsJSON defaultOptions{tagSingleConstructors = True} ''TTagSingleConstructors

tTagSingleConstructors = makeTests_G_TH defaultOptions{tagSingleConstructors = True}
                                        [ CTagSingleConstructors 1
                                        ]

data TPolyTest q = CPolyTest q Int
                 deriving (Show, Typeable, Generic, Generic1)
deriveTsJSON defaultOptions ''TPolyTest

tPolyTest = makeTests_G1_TH defaultOptions $
                            CPolyTest (1 :: Int) 7 :? CPolyTest False 7 :? TestNil

data TPolyTest2 a = CPolyTest2 a (Maybe a)
                    deriving (Show, Typeable, Generic, Generic1)
deriveTsJSON defaultOptions ''TPolyTest2

tPolyTest2 = makeTests_G1_TH defaultOptions $
                             CPolyTest2 (1 :: Int) (Just 1) :? CPolyTest2 (1 :: Int) Nothing :? TestNil

data TPolyTest3 a = CPolyTest3 (Maybe a)
                  deriving (Show, Typeable, Generic, Generic1)
deriveTsJSON defaultOptions ''TPolyTest3

tPolyTest3 = makeTests_G1_TH defaultOptions $
                             (CPolyTest3 Nothing :: TPolyTest3 Int) :? TestNil

data TRecursiveTest a = CRecursiveBase
                      | CRecursiveRecurse a (TRecursiveTest a)
                      deriving (Show, Typeable)
deriveTsJSON defaultOptions ''TRecursiveTest

data THoleTest a = THoleTest
                 deriving (Show, Typeable)

deriveTsTypeable defaultOptions ''THoleTest
                             
data THKTTest a = HKTTestA (a Int)
                | HKTTestB (Maybe (a Text))
                | HKTTestC (THoleTest (a Int))
    deriving (Typeable, Generic)
    
deriveTsTypeable defaultOptions ''THKTTest

data A a = A a
deriveTsTypeable defaultOptions ''A

data B b = B b (A b)
deriveTsTypeable defaultOptions ''B

data C' c1 c2 = C' (c1 Int) c2
deriveTsTypeable defaultOptions ''C'

data D' a d = D' (C' a d)
deriveTsTypeable defaultOptions ''D'

genericTests :: TestTree
genericTests = testGroup "Aeson <-> TS generic deriving isomorphic" $
    tFieldTests ++
    tFieldLabelModifier ++
    tConstructorTagModifier ++
    tAllNullaryToStringTag ++
    tAllNullaryToStringTag' ++
    tOmitNothingFields ++
    tTaggedObject ++
    tUntaggedValue ++
    tObjectWithSingleField ++
    tTwoElemArray ++
    tUnwrapUnaryRecords ++
    tTagSingleConstructors ++
    tPolyTest ++
    tPolyTest2 ++
    tPolyTest3