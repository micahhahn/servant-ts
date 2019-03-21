{-# LANGUAGE OverloadedStrings #-}

module Servant.TS.InstanceTests (
    instanceTests
) where

import Data.Fixed (Fixed, E0(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Ratio (Ratio, (%))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import Data.Version (Version, makeVersion)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Foreign.C.Types (CTime(..))
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS.TestHelpers

instanceTests :: TestTree
instanceTests = testGroup "Aeson <-> TS instance isomorphic"
    [ makeTest True
    , makeTest LT
    , makeTest ()
    , makeTest ('a' :: Char)
    , makeTest (1.0 :: Double)
    , makeTest (1.0 :: Float)
    , makeTest (1 % 2 :: Ratio Int)
    , makeTest (1 :: Fixed E0)
    , makeTest (1 :: Int) 
    , makeTest (1 :: Integer)
    , makeTest (1 :: Natural)
    , makeTest (1 :: Int8)
    , makeTest (1 :: Int16)
    , makeTest (1 :: Int32)
    , makeTest (1 :: Int64)
    , makeTest (1 :: Word)
    , makeTest (1 :: Word8)
    , makeTest (1 :: Word16)
    , makeTest (1 :: Word32)
    , makeTest (1 :: Word64)
    , makeTest (CTime 1)
    , makeTest ("a" :: Text)
    , makeTest ("a" :: LT.Text)
    , makeTest (makeVersion [1,2,3,4])
    , makeTest (Just 1 :: Maybe Int)
    , makeTest (Nothing :: Maybe Int)
    , makeTest (Left 1 :: Either Int Text)
    , makeTest (Right "a" :: Either Int Text)
    , makeTest ([] :: [Int])
    , makeTest ([1] :: [Int])
    {- , makeTest (NonEmpty [1] :: NonEmpty Int) -}
    , makeTest (Identity 1 :: Identity Int)
    , makeTest (Const 1 :: Const Int Text)
    , makeTest (Compose 1 :: Compose Identity Identity Int)
    , makeTest (Pair (Identity 1) (Const "a") :: Product Identity (Const Text) Int)
    ]