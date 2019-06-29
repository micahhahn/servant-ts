{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.TS.Core (
    TsType(..),
    TsTypeF(..),
    TsContext(..),
    flatten
) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable

data TsType = TsVoid
            | TsNever
            | TsNull
            | TsBoolean
            | TsNumber
            | TsString
            | TsStringLiteral Text
            | TsUnion [TsType]
            | TsMap TsType
            | TsNullable TsType
            | TsArray TsType
            | TsObject (HashMap Text TsType)
            | TsTuple [TsType]
            | TsNamedType TypeRep [TsType] TsType
            | TsRef TypeRep [TsType]
            | TsGenericArg Int
            deriving (Show, Eq)

makeBaseFunctor ''TsType

data TsContext a = TsContext a (Map TypeRep TsType)
    deriving (Show, Functor)

instance Applicative TsContext where
    pure a = TsContext a (Map.empty)
    (<*>) (TsContext f m) (TsContext a m') = TsContext (f a) (Map.union m' m)

instance Monad TsContext where
    return = pure
    (>>=) (TsContext a m) f = let (TsContext a' m') = f a
                               in TsContext a' (Map.union m' m)

{- Take a potentially infinitely recursive TsType and abstract out the common TsNamedTypes -}
flatten :: TsType -> TsContext TsType
flatten t = cata f t $ Set.empty
    where f :: TsTypeF (Set TypeRep -> TsContext TsType) -> (Set TypeRep -> TsContext TsType)
          f (TsNamedTypeF tr ts t) = \s -> if Set.member tr s
                                           then TsRef tr <$> sequence (ts <*> return s)
                                           else do let s' = Set.insert tr s
                                                   t' <- t s' 
                                                   ts' <- sequence (ts <*> return s')
                                                   TsContext (TsRef tr ts') (Map.singleton tr t')
          f x = \s -> embed <$> mapM (\f' -> f' s) x