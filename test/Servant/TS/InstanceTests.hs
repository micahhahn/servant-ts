{-# LANGUAGE OverloadedStrings #-}

module Servant.TS.InstanceTests (
    instanceTests
) where

import Data.Fixed (Fixed, E0(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (Ratio, (%))
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Proxy
import Data.Vector (Vector)
import qualified Data.Vector as Vector
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
    {- , makeTest (Seq [1]) -}
    {- , makeTest (Set [1]) -}
    {-, makeTest (IntSet [1]) -}
    {- IntMap -}
    {- Tree -}
    , makeTest (Map.insert 1 "a" Map.empty :: Map Int Text) 
    , makeTest (Vector.fromList [1] :: Vector Int)
    , makeTest (HashMap.fromList [("a", 1)] :: HashMap Text Int)
    , makeTest (LocalTime (ModifiedJulianDay 123456) (TimeOfDay 7 30 0))
    {- NominalDiffTime -}
    {- DiffTime -}
    , makeTest (Proxy :: Proxy Int)
    , makeTest (Tagged 1 :: Tagged Text Int)
    , makeTest ((1, "a") :: (Int, Text))
    , makeTest ((1, "a", False) :: (Int, Text, Bool))
    , makeTest ((1, "a", False, 2) :: (Int, Text, Bool, Int))
    , makeTest ((1, "a", False, 2, "b") :: (Int, Text, Bool, Int, Text))
    , makeTest ((1, "a", False, 2, "b", True) :: (Int, Text, Bool, Int, Text, Bool))
    ]