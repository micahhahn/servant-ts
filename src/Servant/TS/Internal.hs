{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.TS.Internal where

{- import Data.DList (DList) -}
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Compose (Compose)
import Data.Functor.Const (Const)
import Data.Functor.Foldable
import Data.Functor.Identity (Identity)
import Data.Functor.Product (Product)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List (groupBy, sortBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Monoid as Monoid
import qualified Data.Primitive.Array as PM
import qualified Data.Primitive.PrimArray as PM
import qualified Data.Primitive.SmallArray as PM
import qualified Data.Primitive.Types as PM
import qualified Data.Primitive.UnliftedArray as PM
import Data.Proxy
import Data.Ratio (Ratio)
{- import Data.Scientific (Scientific) -}
import qualified Data.Semigroup as Semigroup
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Typeable
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Version (Version)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CTime)
import GHC.Generics
import Numeric.Natural (Natural)

import Servant.Foreign

import Servant.TS.Core



mkTsConName :: forall a p. (Typeable a) => p a -> ConName
mkTsConName p = ConName (mk tyConPackage) (mk tyConModule) (mk tyConName)
    where con = typeRepTyCon . typeRep $ p
          mk f = Text.pack . f $ con

class TsTypeable a where
    tsTypeRep :: Proxy a -> TsType
    {-default tsTypeRep :: forall s. (TsStrategy s a) => Proxy a -> TsType
    tsTypeRep = deriveTsTypeRep @s

data TsDerivingStrategy = TsGeneric 
                        | TsGeneric1

class DeriveTsTypeRep (s :: TsDerivingStrategy) a where
    deriveTsTypeRep :: Proxy a -> TsContext TsType
                        
class DeriveTsTypeRep s a => TsStrategy (s :: TsDerivingStrategy) a | a -> s where

instance (Generic a, Typeable a, TsDatatype (Rep a)) => DeriveTsTypeRep 'TsGeneric a where
    deriveTsTypeRep _ = tsDatatype (from (undefined :: a)) (typeRep (Proxy :: Proxy a)) 

class TsDatatype a where
    tsDatatype :: a p -> TypeRep -> TsContext TsType

instance (Datatype a, TsConstructor c) => TsDatatype (D1 a c) where
    tsDatatype c@(M1 r) t = do
        cons <- tsConstructor r
        let tsType = if length cons == 1 then snd . head $ cons
                                            else TsUnion (makeUnion <$> cons)
        TsContext (TsRef t []) (Map.insert t tsType Map.empty)    

        where makeUnion (n, (TsObject ts')) = TsObject (("tag", TsStringLiteral n): ts')
              makeUnion (n, (TsTuple ts')) = case ts' of
                                                 [] ->  TsObject [("tag", TsStringLiteral n)]
                                                 _ -> TsObject [("tag", TsStringLiteral n), ("contents", TsTuple ts')] 
              makeUnion (n, t) = TsObject [("tag", TsStringLiteral n), ("contents", t)]

class TsConstructor a where
    tsConstructor :: a p -> TsContext [(Text, TsType)]

instance (TsConstructor a, TsConstructor b) => TsConstructor (a :+: b) where
    tsConstructor (_ :: (a :+: b) f) = do
        l <- (tsConstructor (undefined :: a f))
        r <- (tsConstructor (undefined :: b f))
        return $ l ++ r

instance (Constructor a, TsSelector c) => TsConstructor (C1 a c) where
    tsConstructor c@(M1 r) = do
        sels <- tsSelector r
        let n = sanitizeTSName . Text.pack . conName $ c
        let tsType = if conIsRecord c then TsObject sels
                                        else case snd <$> sels of
                                                t:[] -> t
                                                ts -> TsTuple ts
        return [(n, tsType)]

class TsSelector a where
        tsSelector :: a p -> TsContext [(Text, TsType)]

instance (TsSelector a, TsSelector b) => TsSelector (a :*: b) where
    tsSelector (_ :: (a :*: b) f) = do
        l <- tsSelector (undefined :: a f)
        r <- tsSelector (undefined :: b f)
        return $ l ++ r

instance (Selector a, TsTypeable c, Typeable c) => TsSelector (S1 a (K1 b c)) where
    tsSelector q@(M1 (K1 t)) = do
        tsType <- tsTypeRep (Proxy :: Proxy c)
        return [(Text.pack . selName $ q, tsType)]

instance TsSelector U1 where
    tsSelector _ = return []

instance (Generic1 a, Typeable (a p), TsDatatype1 (Rep1 a)) => DeriveTsTypeRep 'TsGeneric1 (a p) where
    deriveTsTypeRep _ = tsDatatype1 (from1 (undefined :: a p)) (typeRep (Proxy :: Proxy (a p))) 

class TsDatatype1 a where
    tsDatatype1 :: a p -> TypeRep -> TsContext TsType
    
instance (Datatype a, TsConstructor1 c) => TsDatatype1 (D1 a c) where
    tsDatatype1 c@(M1 r) t = do
        cons <- tsConstructor1 r
        let tsType = if length cons == 1 then snd . head $ cons
                                         else TsUnion (makeUnion <$> cons)
        TsContext (TsRef t []) (Map.insert t tsType Map.empty)    

        where makeUnion (n, (TsObject ts')) = TsObject (("tag", TsStringLiteral n): ts')
              makeUnion (n, (TsTuple ts')) = case ts' of
                                                 [] ->  TsObject [("tag", TsStringLiteral n)]
                                                 _ -> TsObject [("tag", TsStringLiteral n), ("contents", TsTuple ts')] 
              makeUnion (n, t) = TsObject [("tag", TsStringLiteral n), ("contents", t)]
    
class TsConstructor1 a where
    tsConstructor1 :: a p -> TsContext [(Text, TsType)]

instance (TsConstructor1 a, TsConstructor1 b) => TsConstructor1 (a :+: b) where
    tsConstructor1 (_ :: (a :+: b) f) = do
        l <- (tsConstructor1 (undefined :: a f))
        r <- (tsConstructor1 (undefined :: b f))
        return $ l ++ r

instance (Constructor a, TsSelector1 c) => TsConstructor1 (C1 a c) where
    tsConstructor1 c@(M1 r) = do
        sels <- tsSelector1 r
        let n = sanitizeTSName . Text.pack . conName $ c
        let tsType = if conIsRecord c then TsObject sels
                                        else case snd <$> sels of
                                                t:[] -> t
                                                ts -> TsTuple ts
        return [(n, tsType)]

class TsSelector1 a where
    tsSelector1 :: a p -> TsContext [(Text, TsType)]

instance (TsSelector1 a, TsSelector1 b) => TsSelector1 (a :*: b) where
    tsSelector1 (_ :: (a :*: b) f) = do
        l <- tsSelector1 (undefined :: a f)
        r <- tsSelector1 (undefined :: b f)
        return $ l ++ r

instance (Selector a, TsTypeable c, Typeable c) => TsSelector1 (S1 a (K1 b c)) where
    tsSelector1 q@(M1 (K1 t)) = do
        tsType <- tsTypeRep (Proxy :: Proxy c)
        return [(Text.pack . selName $ q, tsType)]

instance (Selector a) => TsSelector1 (S1 a Par1) where
    tsSelector1 q = return [(Text.pack . selName $ q, TsGenericArg 0)]
-}

instance TsTypeable Bool where
    tsTypeRep _ = TsBoolean

instance TsTypeable Ordering where
    tsTypeRep _ = TsString

instance TsTypeable () where
    tsTypeRep _ = TsArray TsNever

instance TsTypeable Char where
    tsTypeRep _ = TsString

instance TsTypeable Double where
    tsTypeRep _ = TsNumber

{-
instance TsType Number where
    tsType _ = "number"
-}

instance TsTypeable Float where
    tsTypeRep _ = TsNumber

instance (TsTypeable a, Integral a) => TsTypeable (Ratio a) where
    tsTypeRep _ = TsObject $ HashMap.fromList [("denominator", TsNumber), ("numerator", TsNumber)]
    
instance (HasResolution a) => TsTypeable (Fixed a) where
    tsTypeRep _ = TsNumber

instance TsTypeable Int where
    tsTypeRep _ = TsNumber

instance TsTypeable Integer where 
    tsTypeRep _ = TsNumber

instance TsTypeable Natural where
    tsTypeRep _ = TsNumber

instance TsTypeable Int8 where
    tsTypeRep _ = TsNumber

instance TsTypeable Int16 where
    tsTypeRep _ = TsNumber

instance TsTypeable Int32 where
    tsTypeRep _ = TsNumber

instance TsTypeable Int64 where
    tsTypeRep _ = TsNumber

instance TsTypeable Word where
    tsTypeRep _ = TsNumber

instance TsTypeable Word8 where
    tsTypeRep _ = TsNumber

instance TsTypeable Word16 where
    tsTypeRep _ = TsNumber

instance TsTypeable Word32 where
    tsTypeRep _ = TsNumber

instance TsTypeable Word64 where
    tsTypeRep _ = TsNumber

instance TsTypeable CTime where
    tsTypeRep _ = TsNumber

instance TsTypeable Text where
    tsTypeRep _ = TsString

instance TsTypeable LT.Text where
    tsTypeRep _ = TsString

instance TsTypeable Version where
    tsTypeRep _ = TsString

instance (TsTypeable a) => TsTypeable (Maybe a) where
    tsTypeRep _ = TsNullable (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a, TsTypeable b) => TsTypeable (Either a b) where
    tsTypeRep _ = let t = TsDef $ TsUnion [TsObject $ HashMap.fromList [("Left", TsGenericArg 0)], 
                                           TsObject $ HashMap.fromList [("Right", TsGenericArg 1)]]
                      tn = TsTypeName (mkTsConName (Proxy :: Proxy Either)) [] 2
                   in TsNamedType tn [tsTypeRep (Proxy :: Proxy a), tsTypeRep (Proxy :: Proxy b)] t

instance (TsTypeable a) => TsTypeable [a] where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (NonEmpty a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

{-
instance TsType Scientific where
    tsType _ = "number"
-}

{-
instance (TsType a) => TsType (DList a) where
    tsType _ = "Array<" <> tsType (Proxy :: Proxy a) <> ">"
-}

instance (TsTypeable a) => TsTypeable (Identity a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance (TsTypeable a) => TsTypeable (Const a b) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance (TsTypeable (f (g b))) => TsTypeable (Compose f g b) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy (f (g b)))

instance (TsTypeable (f a), TsTypeable (g a)) => TsTypeable (Product f g a) where
    tsTypeRep _ = TsTuple [tsTypeRep (Proxy :: Proxy (f a)), tsTypeRep (Proxy :: Proxy (g a))]

instance (TsTypeable a) => TsTypeable (Seq a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (Set a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance TsTypeable IntSet where
    tsTypeRep _ = TsArray TsNumber

makeMap :: (TsTypeableKey k, TsTypeable v) => Proxy k -> Proxy v -> TsType
makeMap k v = let kt = tsKeyTypeRep k
                  vt = tsTypeRep v
               in case kt of
                      TsString -> TsMap vt
                      _ -> TsArray (TsTuple [kt, vt])

instance (TsTypeable v) => TsTypeable (IntMap v) where
    tsTypeRep _ = makeMap (Proxy :: Proxy Int) (Proxy :: Proxy v)

instance (TsTypeable a, Typeable a) => TsTypeable (Tree a) where
    tsTypeRep _ = let t = TsDef $ TsTuple [TsGenericArg 0, TsArray (tsTypeRep (Proxy :: Proxy (Tree a)))]
                      tn = TsTypeName (mkTsConName (Proxy :: Proxy Tree)) [] 1
                      ts = [tsTypeRep (Proxy :: Proxy a)] 
                   in TsNamedType tn ts t

instance (TsTypeableKey k, TsTypeable v) => TsTypeable (Map.Map k v) where
    tsTypeRep _ = makeMap (Proxy :: Proxy k) (Proxy :: Proxy v)

instance TsTypeable UUID where
    tsTypeRep _ = TsString

instance (TsTypeable a) => TsTypeable (V.Vector a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (VS.Vector a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (VP.Vector a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (VU.Vector a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (HashSet a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeableKey k, TsTypeable v) => TsTypeable (HashMap k v) where
    tsTypeRep _ = makeMap (Proxy :: Proxy k) (Proxy :: Proxy v)

instance (TsTypeable a) => TsTypeable (PM.Array a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (PM.SmallArray a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (PM.PrimArray a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (PM.UnliftedArray a) where
    tsTypeRep _ = TsArray (tsTypeRep (Proxy :: Proxy a))

instance TsTypeable Day where
    tsTypeRep _ = TsString

instance TsTypeable TimeOfDay where
    tsTypeRep _ = TsString

instance TsTypeable LocalTime where
    tsTypeRep _ = TsString

instance TsTypeable ZonedTime where
    tsTypeRep _ = TsString

instance TsTypeable UTCTime where
    tsTypeRep _ = TsString

instance TsTypeable NominalDiffTime where
    tsTypeRep _ = TsNumber

instance TsTypeable DiffTime where
    tsTypeRep _ = TsNumber

instance TsTypeable a => TsTypeable (Monoid.Dual a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance TsTypeable a => TsTypeable (Monoid.First a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance TsTypeable a => TsTypeable (Monoid.Last a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance TsTypeable a => TsTypeable (Semigroup.Min a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance TsTypeable a => TsTypeable (Semigroup.Max a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance TsTypeable a => TsTypeable (Semigroup.First a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance TsTypeable a => TsTypeable (Semigroup.Last a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance TsTypeable a => TsTypeable (Semigroup.WrappedMonoid a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance TsTypeable a => TsTypeable (Semigroup.Option a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

instance TsTypeable (Proxy a) where
    tsTypeRep _ = TsNull

instance TsTypeable b => TsTypeable (Tagged a b) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy b)

append :: TsType -> TsType -> TsType
append (TsTuple ts) t = TsTuple (ts ++ [t])

instance (TsTypeable a, TsTypeable b) => TsTypeable (a, b) where
    tsTypeRep _ = TsTuple [tsTypeRep (Proxy :: Proxy a), tsTypeRep (Proxy :: Proxy b)]

instance (TsTypeable a, TsTypeable b, TsTypeable c) => TsTypeable (a, b, c) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b))) (tsTypeRep (Proxy :: Proxy c))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d) => TsTypeable (a, b, c, d) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c))) (tsTypeRep (Proxy :: Proxy d))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e) => TsTypeable (a, b, c, d, e) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d))) (tsTypeRep (Proxy :: Proxy e))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f) => TsTypeable (a, b, c, d, e, f) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e))) (tsTypeRep (Proxy :: Proxy f))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g) => TsTypeable (a, b, c, d, e, f, g) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e, f))) (tsTypeRep (Proxy :: Proxy g))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h) => TsTypeable (a, b, c, d, e, f, g, h) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e, f, g))) (tsTypeRep (Proxy :: Proxy h))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i) => TsTypeable (a, b, c, d, e, f, g, h, i) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e, f, g, h))) (tsTypeRep (Proxy :: Proxy i))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j) => TsTypeable (a, b, c, d, e, f, g, h, i, j) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e, f, g, h, i))) (tsTypeRep (Proxy :: Proxy j))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j))) (tsTypeRep (Proxy :: Proxy k))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k, TsTypeable l) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k, l) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k))) (tsTypeRep (Proxy :: Proxy l))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k, TsTypeable l, TsTypeable m) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k, l))) (tsTypeRep (Proxy :: Proxy m))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k, TsTypeable l, TsTypeable m, TsTypeable n) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k, l, m))) (tsTypeRep (Proxy :: Proxy n))

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k, TsTypeable l, TsTypeable m, TsTypeable n, TsTypeable o) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    tsTypeRep _ = append (tsTypeRep (Proxy :: Proxy (a, b, c, d, e, f, g, h, i, j, k, l, m, n))) (tsTypeRep (Proxy :: Proxy o))

class TsTypeableKey a where
    tsKeyTypeRep :: Proxy a -> TsType 

instance TsTypeableKey Bool where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Char where 
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Double where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Float where 
    tsKeyTypeRep _ = TsString 

instance TsTypeableKey Int where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Int8 where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Int16 where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Int32 where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Int64 where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Integer where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Natural where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Word where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Word8 where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Word16 where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Word32 where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Word64 where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Text where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey LT.Text where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Version where
    tsKeyTypeRep _ = TsString

{-
instance TsTypeableKey Scientific where
    tsKeyTypeRep _ = TsString
-}

instance TsTypeableKey ZonedTime where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey LocalTime where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey TimeOfDay where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey UTCTime where
    tsKeyTypeRep _ = TsString

instance TsTypeableKey Day where
    tsKeyTypeRep _ = TsString

{-
instance TsTypeableKey UUID where
    tsKeyTypeRep _ = TsString
-}