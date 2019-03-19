{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS

{- Determines if a given JSON value can be represented as a TsType -}
isValid :: TsContext TsType -> Value -> Bool
isValid (TsContext TsBoolean _) (Bool _) = True
isValid (TsContext (TsRef t) m) v = case Map.lookup t m of
    Just t' -> isValid (TsContext t' m) v
    Nothing -> False
isValid _ _ = False

main = defaultMain tests

data X = X Int
    deriving (Generic, Typeable, ToJSON, TsTypeable)

makeTest :: forall a. (ToJSON a, TsTypeable a) => a -> Assertion
makeTest v = let t = tsTypeRep (Proxy :: Proxy a)
                 j = toJSON v
              in isValid t j @? "Failed to unify JSON value with TS type:\n    Value:  " ++ show j ++ "\n    TsType: " ++ show t

tests :: TestTree
tests = testGroup "Aeson <-> TS deriving isomorphic" 
    [ testCase "Basic unnamed type" $ makeTest (X 0)]