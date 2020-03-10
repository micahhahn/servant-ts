{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Servant.TS.Gen (
    tsForAPI,
    defaultTsGenOptions,
    TsGenQuotes(..),
    TsGenIndent(..),
    TsGenOptions(..)
) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (groupBy, sortBy)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Typeable
import Servant.Foreign

import Servant.TS.Core
import Servant.TS.Internal

deriving instance Functor Req
deriving instance Functor Url
deriving instance Functor Segment
deriving instance Functor SegmentType
deriving instance Functor Arg
deriving instance Functor QueryArg
deriving instance Functor HeaderArg

-- Dummy type to parameterize instances
data TypeScript

instance (TsTypeable a) => HasForeignType TypeScript TsType a where
    typeFor _ _ p = (tsTypeRep p)

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

tsForAPI :: (HasForeign TypeScript TsType api, GenerateList TsType (Foreign TsType api)) => Proxy api -> TsGenOptions -> Text
tsForAPI api opts = writeEndpoints opts $ listFromAPI (Proxy :: Proxy TypeScript) (Proxy :: Proxy TsType) api

mkGenericName :: Int -> Text
mkGenericName i = "T" <> (Text.pack . show $ i + 1)

tsUnqualifiedCustomTypeName :: TsTypeName -> Text
tsUnqualifiedCustomTypeName (TsTypeName n ts a) = Text.intercalate "_" (sanitizeTSName (_name n) : (tsUnqualifiedCustomTypeName <$> ts)) <> genericText
    where generics = mkGenericName <$> [0..(a - 1)]
          genericText = if length generics > 0 then "<" <> Text.intercalate ", " generics <> ">" else ""

tsCustomTypeName :: TsTypeName -> [TsRefType] -> Text
tsCustomTypeName (TsTypeName n ns _) as = Text.intercalate "_" ((mn <> "." <> tn) : (tsUnqualifiedCustomTypeName <$> ns)) <> argsText
    where tn = sanitizeTSName . _name $ n
          mn = sanitizeTSName . _module $ n
          args = tsTypeName <$> as
          argsText = if length args > 0 then "<" <> Text.intercalate ", " args <> ">" else ""

tsTypeName :: TsRefType -> Text 
tsTypeName TsVoid = "void"
tsTypeName TsNever = "never"
tsTypeName TsBoolean = "boolean"
tsTypeName TsNumber = "number"
tsTypeName TsString = "string"
tsTypeName (TsStringLiteral n) = "\"" <> n <> "\""
tsTypeName (TsNullable t) = tsTypeName t {- <> "?" -}
tsTypeName (TsNamedType n as _) = tsCustomTypeName n as
tsTypeName (TsArray t) = "Array<" <> tsTypeName t <> ">"
tsTypeName (TsMap t) = "{[key: string]: " <> tsTypeName t <> "}"
tsTypeName (TsTuple ts) = "[" <> Text.intercalate ", " (tsTypeName <$> ts) <> "]"
tsTypeName (TsGenericArg i) = mkGenericName i

makeQuote :: TsGenOptions -> Text
makeQuote opts = case _quotes opts of
                     TsSingleQuotes -> "'"
                     TsDoubleQuotes -> "\""

makeIndent :: TsGenOptions -> Text
makeIndent opts = case _indent opts of
                      TsIndentTab -> "\t"
                      TsIndentSpaces n -> Text.pack . concat . replicate n $ " "

writeEndpoint :: TsGenOptions -> Req (TsContext TsRefType) -> TsContext Text
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

    let args = captures ++ queryArgs ++ bodyArg ++ ["onSuccess: (result: " <> (tsTypeName successType) <> ") => void", "onError: () => void"]
    let jqueryArgs = [("url", url), ("success", "onSuccess"), ("error", "onError"), ("method", quote method)] ++ bodyJQueryArg
    return $ "export function " <> functionName <> "(" <> Text.intercalate ", " args <> "): void\n" <>
             "{\n" <>
             queryPrepare <>
             i' <> "$.ajax({\n" <> i' <> i' <> Text.intercalate (",\n" <> i' <> i') ((\(l, r) -> l <> ": " <> r) <$> jqueryArgs) <> "\n" <> i' <> "});\n" <>
             "}"

    where mapSegment :: TsGenOptions -> SegmentType (TsContext TsRefType) -> Text
          mapSegment _ (Static (PathSegment s)) = "/" <> s
          mapSegment opts (Cap (Arg (PathSegment name) (TsContext t _))) = "/" <> makeQuote opts <> " + encodeURIComponent(" <> writeStringCast name t <> ") + " <> makeQuote opts

          writeStringCast :: Text -> TsRefType -> Text
          writeStringCast n t = case t of
              TsString -> n
              TsNullable a -> writeStringCast n a
              _ -> "String(" <> n <> ")"

          writeTsType :: TsRefType -> Text
          writeTsType TsNever = "never"
          writeTsType TsBoolean = "boolean"
          writeTsType TsNumber = "number"
          writeTsType TsString = "string"
          writeTsType (TsStringLiteral n) = "\"" <> n <> "\"" 
          writeTsType (TsNullable t) = (writeTsType t) <> " | null"

          quote :: Text -> Text
          quote s = makeQuote opts <> s <> makeQuote opts

writeCustomType :: TsGenOptions -> (TsTypeName, TsRefType) -> Text
writeCustomType opts (tr, t) = let prefix = "export type " <> typeName
                                in i' <> prefix <> " = " <> writeCustomTypeDef (Text.length prefix) t <> "\n"
    where i' = makeIndent opts
          typeName = tsUnqualifiedCustomTypeName tr
         
          writeCustomTypeDef :: Int -> TsRefType -> Text
          writeCustomTypeDef i (TsUnion ts) = Text.intercalate ("\n" <> i' <> Text.replicate i " " <> " | ") (writeCustomTypeDef i <$> ts)

          writeCustomTypeDef i (TsObject ts) = "{ " <> Text.intercalate ", " ((\(n, t) -> n <> ": " <> writeCustomTypeDef i t) <$> HashMap.toList ts) <> " }"

          writeCustomTypeDef i (TsTuple ts) = let tuple = Text.intercalate ", " $ writeCustomTypeDef i <$> ts
                                                in if length ts == 1 then tuple else "[" <> tuple <> "]"

          writeCustomTypeDef _ t = tsTypeName t

          writeTypeGuard :: (Text, TsRefType) -> Text
          writeTypeGuard (n, TsObject ts) = let tr = writeCustomTypeDef 0 (TsObject ts)
                                             in i' <> "export function is" <> n <> "($u: " <> typeName <> "): $u is " <> tr <> "\n" <>
                                                i' <> "{\n" <>
                                                i' <> i' <> "let $t = <" <> tr <> ">$u;\n" <>
                                                i' <> i' <> "return " <> Text.intercalate " && " (("$t." <> ) . (<> " !== undefined") <$> HashMap.keys ts) <> ";\n" <>
                                                i' <> "}"

writeCustomTypes :: TsGenOptions -> Map TsTypeName TsRefType -> Text
writeCustomTypes opts m = let as = (\t -> (_module . (\(TsTypeName c _ _) -> c) . fst $ t, t)) <$> Map.assocs m
                              gs = groupBy (\l r -> fst l == fst r) . sortBy (\l r -> fst l `compare` fst r) $ as
                              gs' = (\g -> (fst . head $ g, snd <$> g)) <$> gs
                              t = (\g -> "export namespace " <> fst g <> "\n" <>
                                         "{\n" <>
                                         Text.intercalate "\n" (writeCustomType opts <$> snd g) <>
                                         "}") <$> gs' 
                           in Text.intercalate "\n\n" t

writeEndpoints :: TsGenOptions -> [Req TsType] -> Text 
writeEndpoints opts ts = let (TsContext ts' m) = sequence (writeEndpoint opts <$> ((fmap flatten) <$> ts))
                         in "import * as $ from 'jquery';\n\n" <> 
                            writeCustomTypes opts m <> "\n\n" <>
                            Text.intercalate "\n\n" ts' <> "\n"

sanitizeTSName :: Text -> Text
sanitizeTSName = Text.replace "'" ""