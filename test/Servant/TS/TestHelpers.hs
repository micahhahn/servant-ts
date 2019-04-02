{-# LANGUAGE ScopedTypeVariables #-}

module Servant.TS.TestHelpers (
    makeTest
) where

import Data.Aeson
import Data.Bifunctor
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS

{- Determines if a given JSON value can be represented as a TsType -}
isValid :: TsContext TsType -> Value -> Either (TsContext TsType, Value) ()
isValid (TsContext TsNull _) (Null) = Right ()
isValid (TsContext TsBoolean _) (Bool _) = Right ()
isValid (TsContext TsNumber _) (Number _) = Right ()
isValid (TsContext TsString _) (String _) = Right ()
isValid tt@(TsContext (TsStringLiteral ls) _) (String s) = if ls == s then Right () else Left (tt, (String s))
isValid (TsContext (TsNullable t) m) v = case v of
    Null -> Right ()
    _ -> isValid (TsContext t m) v
isValid (TsContext (TsArray t) m) (Array vs) = if Vector.length vs == 0 
                                               then Right () 
                                               else second Vector.head $ Vector.sequence ((\v -> isValid (TsContext t m) v) <$> vs)
isValid (TsContext (TsUnion ts) m) v = if any (\t -> isRight $ isValid (TsContext t m) v) ts
                                       then Right ()
                                       else Left (TsContext (TsUnion ts) m, v)
isValid tt@(TsContext (TsObject ts) m) (Object m') = if length ts == length m' && all (\x -> HashMap.member x m') (fst <$> ts) 
                                                     then second head . sequence $ (\(n, t) -> isValid (TsContext t m) $ m' HashMap.! n) <$> ts
                                                     else Left (tt, Object m')
isValid (TsContext (TsMap t) m) (Object m') = if all (\v -> isRight $ isValid (TsContext t m) v) (HashMap.elems m')
                                              then Right ()
                                              else Left ((TsContext (TsMap t) m), (Object m'))
isValid tt@(TsContext (TsTuple ts) m) (Array vs) = if (length ts /= length vs)
                                                   then Left (tt, (Array vs))
                                                   else second (const ()) . sequence $ (\(t, v) -> isValid (TsContext t m) v) <$> (zip ts (Vector.toList vs))
isValid tt@(TsContext (TsRef t) m) v = case Map.lookup t m of
    Just t' -> isValid (TsContext t' m) v
    Nothing -> Left (tt, v)
isValid t v = Left (t, v)

makeTest :: forall a. (ToJSON a, Show a, Typeable a, TsTypeable a) => a -> TestTree
makeTest v = let t = tsTypeRep (Proxy :: Proxy a)
                 j = toJSON v
                 a' = case isValid t j of
                        Right _ -> True @? ""
                        Left (TsContext t' _, j') -> False @? "Failed to unify JSON value with TS type:\n" 
                                                           ++ "    Value:  " ++ show j' ++ "\n" 
                                                           ++ "    TsType: " ++ show t' ++ "\n" 
                                                           ++ "Initial types:\n" 
                                                           ++ "    Value:  " ++ show j ++ "\n" 
                                                           ++ "    TsType: " ++ show t
              in testCase (show v ++ " :: " ++ show (typeRep (Proxy :: Proxy a))) a'