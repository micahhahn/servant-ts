{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.TS.Core where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | Avoid requiring Typeable
data ConName = ConName
    { _package :: !Text
    , _module :: !Text
    , _name :: !Text
    } deriving (Show, Eq, Ord)

-- | Contains enough information to uniquely identify a top level typescript definition.
-- | Note that not all type arguments need be named, only those of kind (k -> *). We represent these
-- | in TypeScript as separate named types
data TsTypeName = TsTypeName ConName [TsTypeName]
    deriving (Show, Eq, Ord)

newtype TsDef = TsDef { unTsDef :: TsType }
    deriving (Show, Eq)

newtype TsDefF r = TsDefF { unTsDefF :: r }
    deriving (Functor, Foldable, Traversable)

-- | The core type.  a is always either TsTypeDef or TsTypeRef
data TsTypeBase a = TsVoid
                  | TsNever
                  | TsNull
                  | TsBoolean
                  | TsNumber
                  | TsString
                  | TsStringLiteral Text
                  | TsUnion [TsTypeBase a]
                  | TsMap (TsTypeBase a)
                  | TsNullable (TsTypeBase a)
                  | TsArray (TsTypeBase a)
                  | TsObject (HashMap Text (TsTypeBase a))
                  | TsTuple [TsTypeBase a]
                  | TsNamedType TsTypeName (HashMap Text (TsTypeBase a)) a
                  | TsGenericArg Text
                  deriving (Show, Eq, Functor)

type TsType = TsTypeBase TsDef
type TsRefType = TsTypeBase ()

{- TODO: Fork recursion-schemas to be able to handle this situation -}

data TsTypeBaseF a r = TsVoidF
                     | TsNeverF
                     | TsNullF
                     | TsBooleanF
                     | TsNumberF
                     | TsStringF
                     | TsStringLiteralF Text
                     | TsUnionF [r]
                     | TsMapF r
                     | TsNullableF r
                     | TsArrayF r
                     | TsObjectF (HashMap Text r)
                     | TsTupleF [r]
                     | TsNamedTypeF TsTypeName (HashMap Text r) (a r)
                     | TsGenericArgF Text
                     deriving (Show)

deriving instance Functor (TsTypeBaseF TsDefF)
deriving instance Foldable (TsTypeBaseF TsDefF)
deriving instance Traversable (TsTypeBaseF TsDefF)

type instance Base TsType = TsTypeBaseF TsDefF

instance Recursive TsType where
    project TsVoid = TsVoidF
    project TsNever = TsNeverF
    project TsNull = TsNullF
    project TsBoolean = TsBooleanF
    project TsNumber = TsNumberF
    project TsString = TsStringF
    project (TsStringLiteral a) = TsStringLiteralF a
    project (TsUnion a) = TsUnionF a
    project (TsMap a) = TsMapF a
    project (TsNullable a) = TsNullableF a
    project (TsArray a) = TsArrayF a
    project (TsObject a) = TsObjectF a
    project (TsTuple a) = TsTupleF a
    project (TsNamedType n ts (TsDef t)) = TsNamedTypeF n ts (TsDefF t)
    project (TsGenericArg a) = TsGenericArgF a 

instance Corecursive TsType where
    embed TsVoidF = TsVoid
    embed TsNeverF = TsNever
    embed TsNullF = TsNull
    embed TsBooleanF = TsBoolean
    embed TsNumberF = TsNumber
    embed TsStringF = TsString
    embed (TsStringLiteralF a) = TsStringLiteral a
    embed (TsUnionF a) = TsUnion a
    embed (TsMapF a) = TsMap a
    embed (TsNullableF a) = TsNullable a
    embed (TsArrayF a) = TsArray a
    embed (TsObjectF a) = TsObject a
    embed (TsTupleF a) = TsTuple a
    embed (TsNamedTypeF n ts (TsDefF t)) = TsNamedType n ts (TsDef t)
    embed (TsGenericArgF a) = TsGenericArg a
 
data TsContext a = TsContext a (Map TsTypeName TsRefType)
    deriving (Show, Functor)

instance Applicative TsContext where
    pure a = TsContext a Map.empty
    (<*>) (TsContext f m) (TsContext a m') = TsContext (f a) (Map.union m' m)

{- Laws -}

{- Identity: pure id <*> v = v
   let v = (TsContext a b) 
   then pure id <*> v 
   =    pure id <*> (TsContext a b) 
   =    (TsContext id Map.empty) <*> (TsContext a b) 
   =    (TsContext (id a) (Map.union b Map.empty)
   =    (TxContext a b)
-}

{- Homomorphism: pure f <*> pure x = pure (f x)
   pure f <*> pure x
 = (TsContext f Map.emtpy) <*> (TsContext x Map.empty) 
 = (TsContext (f x) (Map.union Map.empty Map.empty))
 = TsContext (f x) Map.empty 
 = pure (f x)
-}

{- Interchange: u <*> pure y = pure ($ y) <*> u
   u <*> pure y
 = (TsContext a b) <*> (TsContext y Map.empty)
 = TsContext (a y) (Map.union Map.empty b)
 = TsContext (($ y) a) b
 = pure ($ y) <*> u 
-}

{- Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   pure (.) <*> u <*> v <*> w
 = TsContext (.) Map.empty <*> TsContext u' um <*> TsContext v' vm <*> TsContext w' wm
 = TsContext ((.) u') um <*> TsContext v' vm <*> TsContext w' wm
 = TsContext ((.) u' v') (Map.union vm um) <*> TsContext w' wm
 = TsContext ((.) u' v' w') (Map.union wm (Map.union vm um))
 = TsContext (u' (v' w')) (Map.union wm (Map.union vm um))
 = TsContext (u' (v' w')) (Map.union (Map.union wm vm) um) Set union is associative
 = TsContext u' um <*> TsContext (v' w') (Map.union wm vm)
 = TsContext u' um <*> TsContext v' vm <*> TsContext w' wm
 = u <*> (v <*> w)
-}

instance Monad TsContext where
    return = pure
    (>>=) (TsContext a m) f = let (TsContext a' m') = f a
                               in TsContext a' (Map.union m' m)

{- Take a potentially infinitely recursive TsType and abstract out the common TsNamedTypes -}
flatten :: TsType -> TsContext TsRefType
flatten t = cata f t $ Set.empty
    where f :: TsTypeBaseF TsDefF (Set TsTypeName -> TsContext TsRefType) -> (Set TsTypeName -> TsContext TsRefType)
          f (TsNamedTypeF n ts (TsDefF t)) s = if Set.member n s
                                               then (flip $ TsNamedType n) () <$> sequence (fmap ($ s) ts)
                                               else do 
                                                    let s' = Set.insert n s
                                                    t' <- t s' 
                                                    ts' <- sequence (fmap ($ s') ts)
                                                    TsContext (TsNamedType n ts' ()) (Map.singleton n t')
          f TsVoidF _ = pure TsVoid
          f TsNeverF _ = pure TsNever
          f TsNullF _ = pure TsNull
          f TsBooleanF _ = pure TsBoolean
          f TsNumberF _ = pure TsNumber
          f TsStringF _ = pure TsString
          f (TsStringLiteralF a) _ = pure (TsStringLiteral a)
          f (TsUnionF a) s = TsUnion <$> mapM ($ s) a
          f (TsMapF a) s = TsMap <$> a s
          f (TsNullableF a) s = TsNullable <$> a s
          f (TsArrayF a) s = TsArray <$> a s
          f (TsObjectF a) s = TsObject <$> mapM ($ s) a
          f (TsTupleF a) s = TsTuple <$> mapM ($ s) a
          f (TsGenericArgF i) _ = pure (TsGenericArg i)