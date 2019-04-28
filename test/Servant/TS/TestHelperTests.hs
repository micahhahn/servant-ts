{-# LANGUAGE OverloadedStrings #-}

module Servant.TS.TestHelperTests where

import Data.Aeson
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS
import Servant.TS.TestHelpers

validationTests :: TestTree
validationTests = testGroup "isValid tests"
    [ testCase "Union test 1" $ isRight (isValid (TsContext (TsUnion [TsNumber, TsString]) Map.empty) (Number 7)) @? ""
    , testCase "Union test 2" $ isRight (isValid (TsContext (TsUnion [TsNumber, TsString]) Map.empty) (String "a")) @? ""
    , testCase "Tuple test 1" $ isRight (isValid (TsContext (TsTuple [TsNumber, TsString]) Map.empty) (Array (Vector.fromList [(Number 7), (String "a'")]))) @? ""
    , testCase "Tuple test 2" $ isLeft (isValid (TsContext (TsTuple [TsNumber, TsString]) Map.empty) (Array (Vector.fromList [(String "a'"), (Number 7)]))) @? ""
    ]
