{-# LANGUAGE OverloadedStrings #-}

module Servant.TS.TestHelperTests where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS
import Servant.TS.TestHelpers

validationTests :: TestTree
validationTests = testGroup "isValid tests"
    [ testCase "Union test 1" $ isNothing (tsTypecheck (TsUnion [TsNumber, TsString]) (Number 7)) @? ""
    , testCase "Union test 2" $ isNothing (tsTypecheck (TsUnion [TsNumber, TsString]) (String "a")) @? ""
    , testCase "Tuple test 1" $ isNothing (tsTypecheck(TsTuple [TsNumber, TsString]) (Array (Vector.fromList [(Number 7), (String "a'")]))) @? ""
    , testCase "Tuple test 2" $ isJust (tsTypecheck (TsTuple [TsNumber, TsString]) (Array (Vector.fromList [(String "a'"), (Number 7)]))) @? ""
    ]
