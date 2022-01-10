{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.TS.Internal where

import Data.Aeson.TH (Options(..), SumEncoding(..), defaultOptions)
{- import Data.DList (DList) -}
import Data.Bifunctor (first)
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

data TConstructor = TRecord String [(String, TsType)]
                  | TConstructor String [TsType]
    deriving (Show)

data TDatatype = TDatatype TsTypeName [TConstructor]

mkTsType :: Options -> [TsType] -> TDatatype -> TsType
mkTsType opts gs (TDatatype tn cs) =
    let t = case tagSingleConstructors opts || length cs > 1 of
                True -> TsUnion $ (case allNullaryToStringTag opts && all isNullaryCons cs of
                                       True -> TsStringLiteral . conName
                                       False -> \c -> tagCons (conName c) (mkCons c)) <$> cs
                False -> mkCons . head $ cs -- Empty datatype?
     in TsNamedType tn gs (TsDef t)

    where tagCons :: Text -> TsType -> TsType
          tagCons n con = case sumEncoding opts of
              TaggedObject tn cn -> 
                  let conNamePair = ((Text.pack tn), TsStringLiteral n)
                  in case con of
                        TsTuple [] -> TsObject . HashMap.fromList $ [conNamePair]
                        TsObject ts -> TsObject $ uncurry HashMap.insert conNamePair ts
                        x -> TsObject . HashMap.fromList $ conNamePair : [(Text.pack cn, x)]
              UntaggedValue -> con
              ObjectWithSingleField -> TsObject $ HashMap.singleton n con
              TwoElemArray -> TsTuple [TsStringLiteral n, con]

          mkCons :: TConstructor -> TsType
          mkCons (TConstructor _ [t]) = t
          mkCons (TConstructor _ ts) = TsTuple ts
          mkCons (TRecord _ ts) = case unwrapUnaryRecords opts && length ts == 1 of
              True -> snd . head $ ts
              False -> TsObject . HashMap.fromList $ first (Text.pack . fieldLabelModifier opts) <$> ts

          isNullaryCons :: TConstructor -> Bool
          isNullaryCons (TRecord _ []) = True
          isNullaryCons (TConstructor _ []) = True
          isNullaryCons _ = False

          conName :: TConstructor -> Text 
          conName = \case
              TRecord n _ -> f n
              TConstructor n _ -> f n
              where f = Text.pack . constructorTagModifier opts      

mkTsConName :: forall a p. (Typeable a) => p a -> ConName
mkTsConName p = ConName (mk tyConPackage) (mk tyConModule) (mk tyConName)
    where con = typeRepTyCon . typeRep $ p
          mk f = Text.pack . f $ con

mkTsTypeName :: TypeRep -> Int -> TsTypeName
mkTsTypeName t = let (con, args) = splitTyConApp t
                     mk f = Text.pack . f $ con
                     cons =  ConName (mk tyConPackage) (mk tyConModule) (mk tyConName)
                  in TsTypeName cons (($ 0) . mkTsTypeName <$> args)

genericTsTypeable :: forall a. (Typeable a, Generic a, GTsDatatype (Rep a)) => Options -> Proxy a -> TsType
genericTsTypeable opts _ = mkTsType opts [] (TDatatype x cs)
    where x = mkTsTypeName (typeRep (Proxy :: Proxy a)) 0
          cs = tsDataType $ from (undefined :: a)

generic1TsTypeable :: forall a p. (Typeable a, TsTypeable p, Generic1 a, GTsDatatype1 (Rep1 a p)) => Options -> Proxy (a p) -> TsType
generic1TsTypeable opts prox = mkTsType opts [tsTypeRep (Proxy :: Proxy p)] $ TDatatype x cs
    where x = mkTsTypeName (typeRep (Proxy :: Proxy a)) 1
          cs = tsDataType1 $ from1 (undefined :: a p)

class TsTypeable a where
    tsTypeRep :: Proxy a -> TsType
    default tsTypeRep :: (Typeable a, Generic a, GTsDatatype (Rep a)) => Proxy a -> TsType
    tsTypeRep = genericTsTypeable defaultOptions

class GTsDatatype (a :: * -> *) where
    tsDataType :: a p -> [TConstructor]
 
instance (Datatype a, GTsConstructor c) => GTsDatatype (D1 a c) where
    tsDataType (M1 r) = tsConstructor r

class GTsConstructor a where
    tsConstructor :: a p -> [TConstructor]

instance (GTsConstructor a, GTsConstructor b) => GTsConstructor (a :+: b) where
    tsConstructor (_ :: (a :+: b) f) = tsConstructor (undefined :: a f) ++ tsConstructor (undefined :: b f)

instance (Constructor a, GTsSelector c) => GTsConstructor (C1 a c) where
    tsConstructor c@(M1 r) = case conIsRecord c of
                                True -> [TRecord (conName c) (tsSelector r)]
                                False -> [TConstructor (conName c) (snd <$> tsSelector r)]

class GTsSelector a where
    tsSelector :: a p -> [(String, TsType)]

instance (GTsSelector a, GTsSelector b) => GTsSelector (a :*: b) where
    tsSelector (_ :: (a :*: b) f) = tsSelector (undefined :: a f) ++ tsSelector (undefined :: b f)

instance (Selector a, TsTypeable c, Typeable c) => GTsSelector (S1 a (K1 b c)) where
    tsSelector q@(M1 (K1 t)) = [(selName q, tsTypeRep (Proxy :: Proxy c))]

instance GTsSelector U1 where
    tsSelector _ = []

class GTsDatatype1 (a :: *) where
    tsDataType1 :: a -> [TConstructor]

instance forall a (c :: * -> *) (p :: *). (Datatype a, GTsConstructor1 (c p)) => GTsDatatype1 (D1 a c p) where
    tsDataType1 (M1 r) = tsConstructor1 r

class GTsConstructor1 (a :: *) where
    tsConstructor1 :: a -> [TConstructor]

instance forall (a :: * -> *) (b :: * -> *) (p :: *). (GTsConstructor1 (a p), GTsConstructor1 (b p)) => GTsConstructor1 ((a :+: b) p) where
    tsConstructor1 _ = tsConstructor1 (undefined :: a p) ++ tsConstructor1 (undefined :: b p)

instance (Constructor a, GTsSelector1 (c p)) => GTsConstructor1 (C1 a c p) where
    tsConstructor1 c@(M1 r) = case conIsRecord c of
                                  True -> [TRecord (conName c) (tsSelector1 r)]
                                  False -> [TConstructor (conName c) (snd <$> tsSelector1 r)]

class GTsSelector1 (a :: *) where
    tsSelector1 :: a -> [(String, TsType)]

instance forall a b p. (GTsSelector1 (a p), GTsSelector1 (b p)) => GTsSelector1 ((a :*: b) p) where
    tsSelector1 _ = tsSelector1 (undefined :: a p) ++ tsSelector1 (undefined :: b p)

instance (Selector a, TsTypeable c) => GTsSelector1 (S1 a (K1 b c) p) where 
    tsSelector1 q@(M1 (K1 t)) = [(selName q, tsTypeRep (Proxy :: Proxy c))]

instance GTsSelector1 (U1 p) where
    tsSelector1 _ = []

instance (Selector a) => GTsSelector1 (S1 a Par1 p) where
    tsSelector1 q = [(selName q, TsGenericArg 0)]

instance (Selector a, TsTypeable (f p)) => GTsSelector1 (S1 a (Rec1 f) p) where
    tsSelector1 q = [(selName q, t')]
        where t = tsTypeRep (Proxy :: Proxy (f p))
              t' = case t of
                       TsNamedType tn gs d -> TsNamedType tn (take (length gs - 1) gs ++ [TsGenericArg 0]) d
                       x -> x

tsObject :: [(Text, TsType)] -> TsType 
tsObject = undefined
 
(~=) :: forall a. TsTypeable a => Text -> a -> (Text, TsType)
t ~= v = (t, tsTypeRep (Proxy :: Proxy a))

makeTsType :: Typeable a => TsType -> Proxy a -> TsType
makeTsType t p = TsNamedType (mkTsTypeName (typeRep p) 0) [] (TsDef t)

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
