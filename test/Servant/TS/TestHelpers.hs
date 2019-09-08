{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Servant.TS.TestHelpers (
    makeTest,
    tsTypecheck
) where

import Data.Aeson
import Data.Bifunctor
import Data.Either
import Data.Functor.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS

reduceGenerics :: [TsType] -> TsType -> TsType
reduceGenerics ts t = cata f t

    where f :: TsTypeBaseF TsDefF TsType -> TsType
          f (TsGenericArgF i) = ts !! i 
          f x = embed x

type Context = ([Value -> Maybe (TsType, Value)], Value) -> Maybe (TsType, Value)

tsTypecheck :: TsType -> Value -> Maybe (TsType, Value)
tsTypecheck t v = para f t $ ([], v)

    where f :: TsTypeBaseF TsDefF (TsType, Context) -> Context
          f TsNullF (_, Null) = Nothing
          f TsBooleanF (_, (Bool _)) = Nothing
          f TsNumberF (_, (Number _)) = Nothing
          f TsStringF (_, (String _)) = Nothing
          f t@(TsStringLiteralF sl) (_, v@(String s)) = if sl == s then Nothing else mkError t v
          f (TsNullableF _) (_, Null) = Nothing
          f (TsNullableF (_, f')) x = f' x
          f (TsArrayF (_, f')) (gs, Array vs) = firstError $ f' . (gs,) <$> Vector.toList vs
          f t@(TsUnionF ts) x = if any isNothing $ ($ x) . snd <$> ts then Nothing else mkError t (snd x)
          f t@(TsObjectF ts) (gs, v@(Object m)) = if (Set.fromList . HashMap.keys) ts == (Set.fromList . HashMap.keys) m 
                                                 then firstError $ (\((_, f), v) -> f (gs, v)) <$> (HashMap.elems $ HashMap.intersectionWith (,) ts m)
                                                 else mkError t v
          f (TsMapF (_, f')) (gs, Object m) = firstError $ f' . (gs,) <$> HashMap.elems m
          f t@(TsTupleF ts) (gs, v@(Array vs)) = if length ts == length vs
                                                 then firstError $ (\((_, f), v) -> f (gs, v)) <$> zip ts (Vector.toList vs)
                                                 else mkError t v
          f (TsNamedTypeF n ts (TsDefF (_, f))) (gs, v) = f $ ((\(_, f') -> (\v -> f' (gs, v))) <$> ts, v)
          f t@(TsGenericArgF i) (gs, v) = if i < length gs
                                          then (gs !! i) v
                                          else mkError t v
          f t (_, v) = mkError t v

          firstError :: [Maybe a] -> Maybe a
          firstError = find (const True) . mapMaybe id

          mkError :: TsTypeBaseF TsDefF (TsType, Context) -> Value -> Maybe (TsType, Value)
          mkError tF v' = Just (embed . fmap fst $ tF, v')

makeTest :: forall a. (ToJSON a, Show a, Typeable a, TsTypeable a) => a -> TestTree
makeTest v = let t = tsTypeRep (Proxy :: Proxy a)
                 j = toJSON v
                 a' = case tsTypecheck t j of
                        Nothing -> True @? ""
                        Just (t', j') -> False @? "Failed to unify JSON value with TS type:\n" 
                                               ++ "    Value:  " ++ show j' ++ "\n" 
                                               ++ "    TsType: " ++ show t' ++ "\n" 
                                               ++ "Initial types:\n" 
                                               ++ "    Value:  " ++ show j ++ "\n" 
                                               ++ "    TsType: " ++ show t
              in testCase (show v ++ " :: " ++ show (typeRep (Proxy :: Proxy a))) a'