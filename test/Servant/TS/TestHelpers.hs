{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# LANGUAGE PartialTypeSignatures #-}

module Servant.TS.TestHelpers (
    tsTypecheck,
    makeTest,
    makeTests_G_TH,
    makeTests_G1_TH,
    TestList(..)
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
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Datatype
import Test.Tasty
import Test.Tasty.HUnit

import Servant.TS
import Servant.TS.TH

import Data.Foldable (foldrM)

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
          f t@(TsGenericArgF s) (gs, v) = (gs !! s) v
          f t (_, v) = mkError t v

          firstError :: [Maybe a] -> Maybe a
          firstError = find (const True) . mapMaybe id

          mkError :: TsTypeBaseF TsDefF (TsType, Context) -> Value -> Maybe (TsType, Value)
          mkError tF v' = Just (embed . fmap fst $ tF, v')

makeTestInternal :: forall a. (Show a, Typeable a) => a -> TsType -> Value -> Assertion
makeTestInternal v t j = case tsTypecheck t j of
                             Nothing -> True @? ""
                             Just (t', j') -> False @? "Failed to unify JSON value with TS type:\n" 
                                                    ++ "    Value:  " ++ show j' ++ "\n" 
                                                    ++ "    TsType: " ++ show t' ++ "\n" 
                                                    ++ "Initial types:\n" 
                                                    ++ "    Value:  " ++ show j ++ "\n" 
                                                    ++ "    TsType: " ++ show t

makeValueDesc :: forall a. (Show a, Typeable a) => a -> String
makeValueDesc v = show v ++ " :: " ++ show (typeRep (Proxy :: Proxy a))

makeTest :: forall a. (ToJSON a, Show a, Typeable a, TsTypeable a) => a -> TestTree
makeTest v = testCase (makeValueDesc v) $ makeTestInternal v (tsTypeRep (Proxy :: Proxy a)) (toJSON v)

makeTests_G_TH :: forall a. (Show a, Typeable a, Generic a, GTsDatatype (Rep a), GToJSON Zero (Rep a), ToJSON a, TsTypeable a) => Options -> [a] -> [TestTree]
makeTests_G_TH opts vals = [testGroup n $ singleDefTest : (testValue <$> vals)]
    where p = Proxy :: Proxy a
          n = show . typeRepTyCon . typeOf . head $ vals

          singleDefTest = makeSingleDefinitionTest (concat . fmap makeContext $ vals)

          makeContext v = [ DerivingContext (typeRep p) TH (tsTypeRep p)
                          , DerivingContext (typeRep p) G0 (genericTsTypeable opts p) 
                          ]

          testValue v = testGroup (makeValueDesc v) $ 
              [ testCase "G0 <-> Aeson " $ makeTestInternal v (genericTsTypeable opts p) (genericToJSON opts v)
              , testCase "TH <-> Aeson " $ makeTestInternal v (tsTypeRep p) (toJSON v)
              , testCase "G0 = TH" ((tsTypeRep p) @=? (genericTsTypeable opts p))
              ]

data TestList (ls :: [*]) where 
    TestNil :: TestList '[]
    (:?) :: forall a p xs. (Show (a p), TsTypeable (a p), TsTypeable p, Typeable (a p), Typeable a, Generic (a p), GToJSON Zero (Rep (a p)), Generic1 a, GTsDatatype1 (Rep1 a p), GTsDatatype (Rep (a p)), ToJSON (a p)) => a p -> TestList xs -> TestList ((a p) ': xs)

infixr 7 :?

data DerivingMethod = TH
                    | G0
                    | G1
                    deriving (Show, Eq, Enum)

data DerivingContext a = DerivingContext TypeRep DerivingMethod a

makeSingleDefinitionTest :: [DerivingContext TsType] -> TestTree
makeSingleDefinitionTest ts = testCase "Single Definition" $ case foldrM f Map.empty ts of
    Left s -> False @? s 
    Right _ -> True @? ""

    where f :: DerivingContext TsType -> Map TsTypeName (DerivingContext TsDef) -> Either String (Map TsTypeName (DerivingContext TsDef))
          f (DerivingContext tr dm t) m = case t of
              (TsNamedType tn _ td) -> let tn' = stripGenerics tn
                                        in case Map.lookup tn m of
                                               Just (DerivingContext tr' dm' td') -> 
                                                   if td == td' 
                                                   then Right m 
                                                   else Left $ "Found diverging definitions for type:\n"
                                                               ++ "    Type name: " ++ show tn ++ "\n"
                                                               ++ "    Def 1 (" ++ show dm  ++ " - " ++ show tr  ++ "): " ++ show td ++ "\n"
                                                               ++ "    Def 2 (" ++ show dm' ++ " - " ++ show tr' ++ "): " ++ show td'
                                               Nothing -> Right $ Map.insert tn (DerivingContext tr dm td) m 
              _ -> Left $ "Expected TsNamedType, but got " ++ show t

          stripGenerics :: TsTypeName -> TsTypeName
          stripGenerics (TsTypeName cn ts i) = TsTypeName cn ts 0

makeTests_G1_TH :: Options -> TestList xs -> [TestTree]
makeTests_G1_TH opts xs = [testGroup (n xs) $ makeSingleDefinitionTest (unwrapTsTypes xs) : unwrapTests xs]

    where n :: TestList xs -> String
          n (v :? _) = show . typeRepTyCon . typeOf $ v
        
          unwrapTsTypes :: TestList xs -> [DerivingContext TsType]
          unwrapTsTypes TestNil = []
          unwrapTsTypes ((v :: a p) :? vs) =
              let p = Proxy :: Proxy (a p)
                  mkContext = DerivingContext (typeRep p)
                  th = mkContext TH (tsTypeRep p)
                  g0 = mkContext G0 (genericTsTypeable opts p)
                  g1 = mkContext G1 (generic1TsTypeable opts p)
               in [th, g0, g1] ++ unwrapTsTypes vs

          unwrapTests :: TestList xs -> [TestTree]
          unwrapTests TestNil = []
          unwrapTests ((v :: a p) :? vs) = testGroup (show v ++ " :: " ++ show (typeRep p)) [test_TH, test_G0, test_G, test_EQ1] : unwrapTests vs
              where p = Proxy :: Proxy (a p)
                    test_TH = testCase "TH <-> Aeson" $ makeTestInternal v (tsTypeRep p) (toJSON v)
                    test_G0 = testCase "G0 <-> Aeson" $ makeTestInternal v (genericTsTypeable opts p) (genericToJSON opts v)
                    test_G = testCase "G1 <-> Aeson" $ makeTestInternal v (generic1TsTypeable opts p) (genericToJSON opts v)
                    test_EQ1 = testCase "G1 = TH" ((tsTypeRep p) @=? (generic1TsTypeable opts p))