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

{- Determines if a given JSON value can be represented as a TsType -}
isValid :: TsContext TsType -> Value -> Either () (TsContext TsType, Value)
isValid (TsContext TsBoolean _) (Bool _) = Left ()
isValid (TsContext TsNumber _) (Number _) = Left ()
isValid tt@(TsContext (TsRef t) m) v = case Map.lookup t m of
    Just t' -> isValid (TsContext t' m) v
    Nothing -> Right (tt, v)
isValid t v = Right (t, v)

main = defaultMain tests

data X = X Int Bool Text
    deriving (Generic, Typeable, ToJSON, TsTypeable)

makeTest :: forall a. (ToJSON a, TsTypeable a) => a -> Assertion
makeTest v = let t = tsTypeRep (Proxy :: Proxy a)
                 j = toJSON v
              in case isValid t j of
                     Left _ -> True @? ""
                     Right (TsContext t' _, j') -> False @? "Failed to unify JSON value with TS type:\n" 
                                             ++ "    Value:  " ++ show j' ++ "\n" 
                                             ++ "    TsType: " ++ show t' ++ "\n" 
                                             ++ "Initial types:\n" 
                                             ++ "    Value:  " ++ show j ++ "\n" 
                                             ++ "    TsType: " ++ show t

tests :: TestTree
tests = testGroup "Aeson <-> TS deriving isomorphic" 
    [ testCase "Basic unnamed type" $ makeTest (X 0 False "a")]