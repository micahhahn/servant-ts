{-# LANGUAGE ScopedTypeVariables #-}

module Servant.TS.TestHelpers (
    makeTest
) where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS

{- Determines if a given JSON value can be represented as a TsType -}
isValid :: TsContext TsType -> Value -> Either () (TsContext TsType, Value)
isValid (TsContext TsBoolean _) (Bool _) = Left ()
isValid (TsContext TsNumber _) (Number _) = Left ()
isValid (TsContext TsString _) (String _) = Left ()
isValid (TsContext (TsNullable t) m) v = case v of
    Null -> Left ()
    _ -> isValid (TsContext t m) v
isValid tt@(TsContext (TsObject ts) m) (Object m') = if length m == length m' && all (\x -> HashMap.member x m') (fst <$> ts) 
                                                     then case sequence $ (\(n, t) -> isValid (TsContext t m) $ m' HashMap.! n) <$> ts of
                                                        Left _ -> Left ()
                                                        Right ts -> Right $ head ts
                                                     else Right (tt, Object m')
isValid tt@(TsContext (TsRef t) m) v = case Map.lookup t m of
    Just t' -> isValid (TsContext t' m) v
    Nothing -> Right (tt, v)
isValid t v = Right (t, v)

makeTest :: forall a. (ToJSON a, Show a, Typeable a, TsTypeable a) => a -> TestTree
makeTest v = let t = tsTypeRep (Proxy :: Proxy a)
                 j = toJSON v
                 a' = case isValid t j of
                        Left _ -> True @? ""
                        Right (TsContext t' _, j') -> False @? "Failed to unify JSON value with TS type:\n" 
                                                            ++ "    Value:  " ++ show j' ++ "\n" 
                                                            ++ "    TsType: " ++ show t' ++ "\n" 
                                                            ++ "Initial types:\n" 
                                                            ++ "    Value:  " ++ show j ++ "\n" 
                                                            ++ "    TsType: " ++ show t
              in testCase (show v ++ " :: " ++ show (typeRep (Proxy :: Proxy a))) a'