{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Servant.TS.Internal where

{- import Data.DList (DList) -}
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Compose (Compose)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Functor.Product (Product)
import qualified Data.HashMap.Strict as HMS
import Data.HashSet (HashSet)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List (groupBy, sortBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
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

-- Dummy type to parameterize instances
data TypeScript

instance (TsTypeable a) => HasForeignType TypeScript (TsContext TsType) a where
    typeFor _ _ p = tsTypeRep p

data TsGenQuotes = TsSingleQuotes
                 | TsDoubleQuotes
                 deriving (Eq, Ord, Enum, Show)

data TsGenIndent = TsIndentTab
                 | TsIndentSpaces Int
                 deriving (Eq, Ord, Show)

data TsGenOptions = TsGenOptions
    { _quotes :: TsGenQuotes
    , _indent :: TsGenIndent
    } deriving (Show)

defaultTsGenOptions :: TsGenOptions
defaultTsGenOptions = TsGenOptions TsSingleQuotes (TsIndentSpaces 4)

tsForAPI :: (HasForeign TypeScript (TsContext TsType) api, GenerateList (TsContext TsType) (Foreign (TsContext TsType) api)) => Proxy api -> TsGenOptions -> Text
tsForAPI api opts = writeEndpoints opts $ listFromAPI (Proxy :: Proxy TypeScript) (Proxy :: Proxy (TsContext TsType)) api

{- Using 'data' in jquery options potentially incorrect? -}

{- Currently we are outputting every use of a polymorphic type by adding the type arguments as a suffix to the name. 
   AFAIK Typeable does not give us a way to determine if a type argument is of kind * or k -> *. That shouldn't matter
   though in this case because we will only be able to work backwards from concrete types if the type argument is used so
   we can effectively ignore phantom types and higher kinded types alike.
-}

tsUnqualifiedCustomTypeName :: TypeRep -> Text
tsUnqualifiedCustomTypeName t = let tn = sanitizeTSName . Text.pack . tyConName . typeRepTyCon $ t
                                 in Text.intercalate "_" (tn : (tsUnqualifiedCustomTypeName <$> typeRepArgs t))

tsCustomTypeName :: TypeRep -> Text
tsCustomTypeName t = let tn = sanitizeTSName . Text.pack . tyConName . typeRepTyCon $ t
                         mn = sanitizeTSName . Text.pack . tyConModule . typeRepTyCon $ t
                      in Text.intercalate "_" ((mn <> "." <> tn) : (tsUnqualifiedCustomTypeName <$> typeRepArgs t))

tsTypeName :: TsType -> Text 
tsTypeName TsVoid = "void"
tsTypeName TsNever = "never"
tsTypeName TsBoolean = "boolean"
tsTypeName TsNumber = "number"
tsTypeName TsString = "string"
tsTypeName (TsLiteral n) = n
tsTypeName (TsStringLiteral n) = "\"" <> n <> "\""
tsTypeName (TsNullable t) = tsTypeName t {- <> "?" -}
tsTypeName (TsRef t) = tsCustomTypeName t 
tsTypeName (TsArray t) = "Array<" <> tsTypeName t <> ">"
tsTypeName (TsMap t) = "{[key: string]: " <> tsTypeName t <> "}"

makeQuote :: TsGenOptions -> Text
makeQuote opts = case _quotes opts of
                     TsSingleQuotes -> "'"
                     TsDoubleQuotes -> "\""

makeIndent :: TsGenOptions -> Text
makeIndent opts = case _indent opts of
                      TsIndentTab -> "\t"
                      TsIndentSpaces n -> Text.pack . concat . take n . repeat $ " "

writeEndpoint :: TsGenOptions -> Req (TsContext TsType) -> TsContext Text
writeEndpoint opts t = do
    let i' = makeIndent opts
    let functionName = mconcat . map Text.toTitle . unFunctionName . _reqFuncName $ t
    let method = TE.decodeUtf8 $ _reqMethod t
    successType <- maybe (return TsVoid) id (_reqReturnType t)
    captures <- sequence [type' >>= (\t -> return $ name <> ": " <> tsTypeName t) | Cap (Arg (PathSegment name) type') <- (unSegment <$> (_path . _reqUrl $ t))]
    (bodyArg, bodyJQueryArg) <- maybe (return ([], [])) (fmap (\t -> (["$body: " <> tsTypeName t], [("data", "$body")]))) $ _reqBody t
    
    q <- sequence $ (\a -> do
                        t <- _argType . _queryArgName $ a
                        return (unPathSegment . _argName . _queryArgName $ a, t, _queryArgType a)
                    ) <$> (_queryStr . _reqUrl $ t)
    
    let queryArgs = if null q then [] else [("$query: {" <> Text.intercalate ", " ((\(n, t, _) -> n <> ": " <> tsTypeName t) <$> q) <> "}")]

    let checkArg (n, t, at) = let param = "$query." <> n
                               in case at of
                                      Flag -> i' <> "if (" <> param <> " === true)\n" <>
                                              i' <> i' <> "$queryArgs.push(" <> quote n <> ");\n"
                                      Normal -> i' <> "if (" <> param <> " !== undefined)\n" <>
                                                i' <> i' <> "$queryArgs.push(" <> quote (n <> "=") <> " + encodeURIComponent(" <> writeStringCast param t <> "));\n"
                                      List -> i' <> "if (" <> param <> " !== undefined)\n" <>
                                              i' <> i' <> "$queryArgs.push(..." <> param <> ".map(x => " <> quote (n <> "=") <> " + encodeURIComponent(" <> writeStringCast "x" t <> ")));\n"

    let queryPrepare = if null q then ""
                                 else i' <> "let $queryArgs : string[] = [];\n\n" <>
                                      Text.intercalate "\n" (checkArg <$> q) <> "\n" <>
                                      i' <> "let $queryString = $queryArgs.length == 0 ? " <> quote "" <> " : " <> quote "?" <> " + $queryArgs.join(" <> quote "&" <> ");\n\n"

    let url = quote (mconcat (mapSegment opts . unSegment <$> (_path . _reqUrl $ t))) <>
              if null q then "" else " + $queryString"

    let args = captures ++ queryArgs ++ bodyArg ++ ["onSuccess: (result: " <> tsTypeName successType <> ") => void", "onError: () => void"]
    let jqueryArgs = [("url", url), ("success", "onSuccess"), ("error", "onError"), ("method", quote method)] ++ bodyJQueryArg
    return $ "export function " <> functionName <> "(" <> Text.intercalate ", " args <> "): void\n" <>
             "{\n" <>
             queryPrepare <>
             i' <> "$.ajax({\n" <> i' <> i' <> Text.intercalate (",\n" <> i' <> i') ((\(l, r) -> l <> ": " <> r) <$> jqueryArgs) <> "\n" <> i' <> "});\n" <>
             "}"

    where mapSegment :: TsGenOptions -> SegmentType (TsContext TsType) -> Text
          mapSegment _ (Static (PathSegment s)) = "/" <> s
          mapSegment opts (Cap (Arg (PathSegment name) (TsContext t _))) = "/" <> makeQuote opts <> " + encodeURIComponent(" <> writeStringCast name t <> ") + " <> makeQuote opts

          writeStringCast :: Text -> TsType -> Text
          writeStringCast n t = case t of
              TsString -> n
              TsNullable a -> writeStringCast n a
              _ -> "String(" <> n <> ")"

          writeTsType :: TsType -> Text
          writeTsType TsNever = "never"
          writeTsType TsBoolean = "boolean"
          writeTsType TsNumber = "number"
          writeTsType TsString = "string"
          writeTsType (TsStringLiteral n) = "\"" <> n <> "\"" 
          {- writeTsType (TsTaggedUnion _ ts) = Text.intercalate " | " (writeTsType <$> ts) -}
          writeTsType (TsNullable t) = (writeTsType t) <> " | null"

          quote :: Text -> Text
          quote s = makeQuote opts <> s <> makeQuote opts

untagUnion :: TsType -> TsType
untagUnion (TsTaggedUnion tn ts) = TsUnion $ (\(n, t) -> case t of 
                                                             (TsObject ts') -> TsObject ((tn, TsStringLiteral n): ts')
                                                             (TsTuple ts') -> TsObject [(tn, TsStringLiteral n), ("contents", TsTuple ts')]
                                             ) <$> ts 
untagUnion t = t

writeCustomType :: TsGenOptions -> (TypeRep, TsType) -> Text
writeCustomType opts (tr, t) = let prefix = "export type " <> typeName
                                in i' <> prefix <> " = " <> writeCustomTypeDef (Text.length prefix) (untagUnion t) <> "\n"
    where i' = makeIndent opts
          typeName = tsUnqualifiedCustomTypeName tr
         
          writeCustomTypeDef :: Int -> TsType -> Text
          writeCustomTypeDef i (TsUnion ts) = Text.intercalate ("\n" <> i' <> Text.replicate i " " <> " | ") (writeCustomTypeDef i <$> ts)

          writeCustomTypeDef i (TsUntaggedUnion ts) = let typeRep = Text.intercalate ("\n\t" <> Text.replicate i " " <> " | ") (writeCustomTypeDef i . snd <$> ts)
                                                       in Text.intercalate "\n\n" (typeRep : (writeTypeGuard <$> ts))

          writeCustomTypeDef i (TsObject ts) = "{ " <> Text.intercalate ", " ((\(n, t) -> n <> ": " <> writeCustomTypeDef i t) <$> ts) <> " }"

          writeCustomTypeDef i (TsTuple ts) = let tuple = Text.intercalate ", " $ writeCustomTypeDef i <$> ts
                                                in if length ts == 1 then tuple else "[" <> tuple <> "]"

          writeCustomTypeDef _ t = tsTypeName t

          makeQualifiedType :: Text -> TypeRep -> Text
          makeQualifiedType n ts = Text.intercalate "_" (n : (tsUnqualifiedCustomTypeName <$> typeRepArgs ts))

          writeTypeGuard :: (Text, TsType) -> Text
          writeTypeGuard (n, TsObject ts) = let tr = writeCustomTypeDef 0 (TsObject ts)
                                             in i' <> "export function is" <> n <> "($u: " <> typeName <> "): $u is " <> tr <> "\n" <>
                                                i' <> "{\n" <>
                                                i' <> i' <> "let $t = <" <> tr <> ">$u;\n" <>
                                                i' <> i' <> "return " <> Text.intercalate " && " (("$t." <> ) . (<> " !== undefined") . fst <$> ts) <> ";\n" <>
                                                i' <> "}"

writeCustomTypes :: TsGenOptions -> Map TypeRep TsType -> Text
writeCustomTypes opts m = let as = (\t -> (Text.pack . tyConModule . typeRepTyCon . fst $ t, t)) <$> Map.assocs m
                              gs = groupBy (\l r -> fst l == fst r) . sortBy (\l r -> fst l `compare` fst r) $ as
                              gs' = (\g -> (fst . head $ g, snd <$> g)) <$> gs
                              t = (\g -> "export namespace " <> fst g <> "\n" <>
                                         "{\n" <>
                                         Text.intercalate "\n" (writeCustomType opts <$> snd g) <>
                                         "}") <$> gs' 
                           in Text.intercalate "\n\n" t

writeEndpoints :: TsGenOptions -> [Req (TsContext TsType)] -> Text 
writeEndpoints opts ts = let (TsContext ts' m) = sequence (writeEndpoint opts <$> ts)
                         in "import * as $ from 'jquery';\n\n" <> 
                            writeCustomTypes opts m <> "\n\n" <>
                            Text.intercalate "\n\n" ts' <> "\n"

data TsType = TsVoid
            | TsNever
            | TsNull
            | TsBoolean
            | TsNumber
            | TsString
            | TsLiteral Text
            | TsStringLiteral Text
            | TsUnion [TsType]
            | TsUntaggedUnion [(Text, TsType)]
            | TsTaggedUnion Text {- tag field name -} [(Text, TsType)]
            | TsMap TsType
            | TsNullable TsType
            | TsArray TsType
            | TsObject [(Text, TsType)]
            | TsTuple [TsType]
            | TsRef TypeRep
            | TsGenericArg Int
    deriving (Show)

{- TypeRep key will contain Arity positions -}

data TsContext a = TsContext a (Map TypeRep TsType)
    deriving (Show, Functor)

instance Applicative TsContext where
    pure a = TsContext a (Map.empty)
    (<*>) (TsContext f m) (TsContext a m') = TsContext (f a) (Map.union m m')

instance Monad TsContext where
    return = pure
    (>>=) (TsContext a m) f = let (TsContext a' m') = f a
                               in TsContext a' (Map.union m m')

sanitizeTSName :: Text -> Text
sanitizeTSName = Text.replace "'" ""

class TsTypeable a where
    tsTypeRep :: Proxy a -> TsContext TsType
    default tsTypeRep :: (Generic a, Typeable a, TsDatatype (Rep a)) => Proxy a -> TsContext TsType
    tsTypeRep p = tsDatatype (from (undefined :: a)) (typeRep p)

class TsDatatype a where
    tsDatatype :: a p -> TypeRep -> TsContext TsType

instance (Datatype a, TsConstructor c) => TsDatatype (D1 a c) where
    tsDatatype c@(M1 r) t = do
        cons <- tsConstructor r
        let tsType = if length cons == 1 then snd . head $ cons
                                         else TsTaggedUnion "tag" cons
        TsContext (TsRef t) (Map.insert t tsType Map.empty)

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

instance TsTypeable Bool where
    tsTypeRep _ = return TsBoolean

instance TsTypeable Ordering where
    tsTypeRep _ = return TsString

instance TsTypeable () where
    tsTypeRep _ = return $ TsArray TsNever

instance TsTypeable Char where
    tsTypeRep _ = return TsString

instance TsTypeable Double where
    tsTypeRep _ = return TsNumber

{-
instance TsType Number where
    tsType _ = "number"
-}

instance TsTypeable Float where
    tsTypeRep _ = return TsNumber

instance (TsTypeable a, Integral a) => TsTypeable (Ratio a) where
    tsTypeRep _ = return $ TsObject [("denominator", TsNumber), ("numerator", TsNumber)]
    
instance (HasResolution a) => TsTypeable (Fixed a) where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int where
    tsTypeRep _ = return TsNumber

instance TsTypeable Integer where 
    tsTypeRep _ = return TsNumber

instance TsTypeable Natural where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int8 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int16 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int32 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int64 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word8 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word16 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word32 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word64 where
    tsTypeRep _ = return TsNumber

instance TsTypeable CTime where
    tsTypeRep _ = return TsNumber

instance TsTypeable Text where
    tsTypeRep _ = return TsString

instance TsTypeable LT.Text where
    tsTypeRep _ = return TsString

instance TsTypeable Version where
    tsTypeRep _ = return TsString

instance (TsTypeable a) => TsTypeable (Maybe a) where
    tsTypeRep _ = TsNullable <$> (tsTypeRep (Proxy :: Proxy a))

instance (Typeable a, TsTypeable a, Typeable b, TsTypeable b) => TsTypeable (Either a b) where
    tsTypeRep _ = do 
        l <- tsTypeRep (Proxy :: Proxy a)
        r <- tsTypeRep (Proxy :: Proxy b)
        let t = TsUnion [TsObject [("Left", l)], TsObject [("Right", r)]]
        let tr = typeRep (Proxy :: Proxy (Either a b))
        _ <- TsContext () $ Map.insert tr t Map.empty
        return (TsRef tr)

instance (TsTypeable a) => TsTypeable [a] where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (NonEmpty a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

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
    tsTypeRep _ = do
        l <- tsTypeRep (Proxy :: Proxy (f a))
        r <- tsTypeRep (Proxy :: Proxy (g a))
        return $ TsTuple [l, r]

instance (TsTypeable a) => TsTypeable (Seq a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (Set a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance TsTypeable IntSet where
    tsTypeRep _ = return $ TsArray TsNumber

makeMap :: (TsTypeableKey k, TsTypeable v) => Proxy k -> Proxy v -> TsContext TsType
makeMap k v = do
    kt <- tsKeyTypeRep k
    vt <- tsTypeRep v
    case kt of
        TsString -> return $ TsMap vt
        _ -> return $ TsArray (TsTuple [kt, vt])

instance (TsTypeable v) => TsTypeable (IntMap v) where
    tsTypeRep _ = makeMap (Proxy :: Proxy Int) (Proxy :: Proxy v)

{- instance TsType Tree where -}

instance (TsTypeableKey k, TsTypeable v) => TsTypeable (Map.Map k v) where
    tsTypeRep _ = makeMap (Proxy :: Proxy k) (Proxy :: Proxy v)

instance TsTypeable UUID where
    tsTypeRep _ = return TsString

instance (TsTypeable a) => TsTypeable (V.Vector a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (VS.Vector a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (VP.Vector a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (VU.Vector a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (HashSet a) where
    tsTypeRep _ = TsArray <$> tsTypeRep (Proxy :: Proxy a)

instance (TsTypeableKey k, TsTypeable v) => TsTypeable (HMS.HashMap k v) where
    tsTypeRep _ = makeMap (Proxy :: Proxy k) (Proxy :: Proxy v)

instance (TsTypeable a) => TsTypeable (PM.Array a) where
    tsTypeRep _ = TsArray <$> tsTypeRep (Proxy :: Proxy a)

instance (TsTypeable a) => TsTypeable (PM.SmallArray a) where
    tsTypeRep _ = TsArray <$> tsTypeRep (Proxy :: Proxy a)

instance (TsTypeable a) => TsTypeable (PM.PrimArray a) where
    tsTypeRep _ = TsArray <$> tsTypeRep (Proxy :: Proxy a)

instance (TsTypeable a) => TsTypeable (PM.UnliftedArray a) where
    tsTypeRep _ = TsArray <$> tsTypeRep (Proxy :: Proxy a)

instance TsTypeable Day where
    tsTypeRep _ = return TsString

instance TsTypeable TimeOfDay where
    tsTypeRep _ = return TsString

instance TsTypeable LocalTime where
    tsTypeRep _ = return TsString

instance TsTypeable ZonedTime where
    tsTypeRep _ = return TsString

instance TsTypeable UTCTime where
    tsTypeRep _ = return TsString

instance TsTypeable NominalDiffTime where
    tsTypeRep _ = return TsNumber

instance TsTypeable DiffTime where
    tsTypeRep _ = return TsNumber

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
    tsTypeRep _ = return TsNull

instance TsTypeable b => TsTypeable (Tagged a b) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy b)

instance (TsTypeable a, TsTypeable b) => TsTypeable (a, b) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        return $ TsTuple [a', b']

instance (TsTypeable a, TsTypeable b, TsTypeable c) => TsTypeable (a, b, c) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        return $ TsTuple [a', b', c']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d) => TsTypeable (a, b, c, d) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        return $ TsTuple [a', b', c', d']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e) => TsTypeable (a, b, c, d, e) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        return $ TsTuple [a', b', c', d', e']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f) => TsTypeable (a, b, c, d, e, f) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        return $ TsTuple [a', b', c', d', e', f']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g) => TsTypeable (a, b, c, d, e, f, g) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        g' <- tsTypeRep (Proxy :: Proxy g)
        return $ TsTuple [a', b', c', d', e', f', g']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h) => TsTypeable (a, b, c, d, e, f, g, h) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        g' <- tsTypeRep (Proxy :: Proxy g)
        h' <- tsTypeRep (Proxy :: Proxy h)
        return $ TsTuple [a', b', c', d', e', f', g', h']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i) => TsTypeable (a, b, c, d, e, f, g, h, i) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        g' <- tsTypeRep (Proxy :: Proxy g)
        h' <- tsTypeRep (Proxy :: Proxy h)
        i' <- tsTypeRep (Proxy :: Proxy i)
        return $ TsTuple [a', b', c', d', e', f', g', h', i']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j) => TsTypeable (a, b, c, d, e, f, g, h, i, j) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        g' <- tsTypeRep (Proxy :: Proxy g)
        h' <- tsTypeRep (Proxy :: Proxy h)
        i' <- tsTypeRep (Proxy :: Proxy i)
        j' <- tsTypeRep (Proxy :: Proxy j)
        return $ TsTuple [a', b', c', d', e', f', g', h', i', j']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        g' <- tsTypeRep (Proxy :: Proxy g)
        h' <- tsTypeRep (Proxy :: Proxy h)
        i' <- tsTypeRep (Proxy :: Proxy i)
        j' <- tsTypeRep (Proxy :: Proxy j)
        k' <- tsTypeRep (Proxy :: Proxy k)
        return $ TsTuple [a', b', c', d', e', f', g', h', i', j', k']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k, TsTypeable l) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k, l) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        g' <- tsTypeRep (Proxy :: Proxy g)
        h' <- tsTypeRep (Proxy :: Proxy h)
        i' <- tsTypeRep (Proxy :: Proxy i)
        j' <- tsTypeRep (Proxy :: Proxy j)
        k' <- tsTypeRep (Proxy :: Proxy k)
        l' <- tsTypeRep (Proxy :: Proxy l)
        return $ TsTuple [a', b', c', d', e', f', g', h', i', j', k', l']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k, TsTypeable l, TsTypeable m) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        g' <- tsTypeRep (Proxy :: Proxy g)
        h' <- tsTypeRep (Proxy :: Proxy h)
        i' <- tsTypeRep (Proxy :: Proxy i)
        j' <- tsTypeRep (Proxy :: Proxy j)
        k' <- tsTypeRep (Proxy :: Proxy k)
        l' <- tsTypeRep (Proxy :: Proxy l)
        m' <- tsTypeRep (Proxy :: Proxy m)
        return $ TsTuple [a', b', c', d', e', f', g', h', i', j', k', l', m']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k, TsTypeable l, TsTypeable m, TsTypeable n) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        g' <- tsTypeRep (Proxy :: Proxy g)
        h' <- tsTypeRep (Proxy :: Proxy h)
        i' <- tsTypeRep (Proxy :: Proxy i)
        j' <- tsTypeRep (Proxy :: Proxy j)
        k' <- tsTypeRep (Proxy :: Proxy k)
        l' <- tsTypeRep (Proxy :: Proxy l)
        m' <- tsTypeRep (Proxy :: Proxy m)
        n' <- tsTypeRep (Proxy :: Proxy n)
        return $ TsTuple [a', b', c', d', e', f', g', h', i', j', k', l', m', n']

instance (TsTypeable a, TsTypeable b, TsTypeable c, TsTypeable d, TsTypeable e, TsTypeable f, TsTypeable g, TsTypeable h, TsTypeable i, TsTypeable j, TsTypeable k, TsTypeable l, TsTypeable m, TsTypeable n, TsTypeable o) => TsTypeable (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    tsTypeRep _ = do
        a' <- tsTypeRep (Proxy :: Proxy a)
        b' <- tsTypeRep (Proxy :: Proxy b)
        c' <- tsTypeRep (Proxy :: Proxy c)
        d' <- tsTypeRep (Proxy :: Proxy d)
        e' <- tsTypeRep (Proxy :: Proxy e)
        f' <- tsTypeRep (Proxy :: Proxy f)
        g' <- tsTypeRep (Proxy :: Proxy g)
        h' <- tsTypeRep (Proxy :: Proxy h)
        i' <- tsTypeRep (Proxy :: Proxy i)
        j' <- tsTypeRep (Proxy :: Proxy j)
        k' <- tsTypeRep (Proxy :: Proxy k)
        l' <- tsTypeRep (Proxy :: Proxy l)
        m' <- tsTypeRep (Proxy :: Proxy m)
        n' <- tsTypeRep (Proxy :: Proxy n)
        o' <- tsTypeRep (Proxy :: Proxy o)
        return $ TsTuple [a', b', c', d', e', f', g', h', i', j', k', l', m', n', o']

class TsTypeableKey a where
    tsKeyTypeRep :: Proxy a -> TsContext TsType 

instance TsTypeableKey Bool where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Char where 
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Double where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Float where 
    tsKeyTypeRep _ = return TsString 

instance TsTypeableKey Int where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Int8 where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Int16 where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Int32 where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Int64 where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Integer where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Natural where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Word where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Word8 where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Word16 where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Word32 where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Word64 where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Text where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey LT.Text where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Version where
    tsKeyTypeRep _ = return TsString

{-
instance TsTypeableKey Scientific where
    tsKeyTypeRep _ = return TsString
-}

instance TsTypeableKey ZonedTime where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey LocalTime where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey TimeOfDay where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey UTCTime where
    tsKeyTypeRep _ = return TsString

instance TsTypeableKey Day where
    tsKeyTypeRep _ = return TsString

{-
instance TsTypeableKey UUID where
    tsKeyTypeRep _ = return TsString
-}