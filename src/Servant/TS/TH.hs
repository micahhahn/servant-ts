{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Servant.TS.TH (
    deriveTsJSON,
    deriveTsTypeable
) where

import Data.Aeson.TH (Options(..), SumEncoding(..), deriveJSON)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

import Data.Map (Map)
import qualified Data.Map as Map

import Servant.TS.Internal (TsTypeable(..), TsType(..), TsContext(..))

deriveTsJSON :: Options -> Name -> Q [Dec]
deriveTsJSON opts name = do
    ts <- deriveTsTypeable opts name
    js <- deriveJSON opts name
    return (ts ++ js) 

deriveTsTypeable :: Options -> Name -> Q [Dec]
deriveTsTypeable opts name = do
    DatatypeInfo { datatypeVars = vars
                 , datatypeCons = cons } <- reifyDatatype name
    stvs <- isExtEnabled ScopedTypeVariables
    _ <- if not stvs && length vars > 0 
         then fail $ "You must have the ScopedTypeVariables language extension enabled to derive TsTypeable for polymorphic type " ++ (nameBase name) ++ "." 
         else return ()
    mkInstanceD vars name (mkTsTypeRep vars cons)

    where mkInstanceD :: [Type] -> Name -> Q Dec -> Q [Dec] {- Q  instance (TsTypeable a, ...) => TsTypable x where  -}
          mkInstanceD ts n d = do
              con <- conT n
              n' <- [t| TsTypeable $(return $ foldl AppT (ConT n) ((\(SigT v _) -> v) <$> ts) ) |]
              d' <- d
              let ts' = (\(SigT v _) -> classPred ''TsTypeable [v]) <$> ts
              return [InstanceD Nothing ts' n' [d']]
                
          {- Collects all subtypes in a data declaration and generates one unqiue var name for each -}
          collectSubtypes :: Map Type ExpQ -> [ConstructorInfo] -> Q (Map Type Name)
          collectSubtypes m cs = let subTypes = concat (constructorFields <$> cs)
                                     filtered = filter (\x -> True {- not $ Map.member x m -}) subTypes
                                  in Map.fromList <$> (sequence $ (\t -> (t,) <$> newName "ts") <$> filtered)
          
          mkTsTypeRep :: [Type] -> [ConstructorInfo] -> Q Dec
          mkTsTypeRep vars cons = do
              let generics = (\(SigT v _) -> v) <$> vars 
              let genericArgs = Map.fromList $ (\x -> (snd x, [| TsGenericArg $(return . LitE . IntegerL . fst $ x) |])) <$> zip [0..] ((\(SigT v _) -> v) <$> vars) 
              ts <- collectSubtypes genericArgs cons
              let body = [| $(if tagSingleConstructors opts || length cons > 1
                              then let mk = if allNullaryToStringTag opts && all isNullaryCons cons then mkNullaryStringConsE else mkTaggedTypeE (genericArgs, ts)
                                    in [| TsUnion $(ListE <$> sequence (mk <$> cons)) |]
                              else mkTypeE (genericArgs, ts) (head cons))
                          |]
              
              let binds = (\(t, n) -> bindS (varP n) [| tsTypeRep (Proxy :: Proxy $(return t)) |]) <$> (Map.assocs ts)
              let context = bindS wildP [| TsContext () (Map.singleton (typeRep (Proxy :: Proxy $(conT name))) $body) |]
              let ref = [| return $ TsRef (typeRep (Proxy :: Proxy $(conT name))) $(return . ListE $ VarE . (ts Map.!) <$> generics) |]
              let dos = doE (binds ++ [context, noBindS ref])
              funD 'tsTypeRep [clause [wildP] (normalB dos) []]
            
          mkTypeE :: (Map Type ExpQ, Map Type Name) -> ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkTypeE m c = case constructorVariant c of
                            NormalConstructor -> makeTupleE (mkNormalFieldE m <$> constructorFields c)
                            RecordConstructor ns -> makeRecordE (mkRecordFieldE m <$> zip ns (constructorFields c))

          makeTupleE :: [Q Exp] -> Q Exp {- [Q TsContext TsType] -> Q (TsContext TsType) -}
          makeTupleE ts = case ts of
                              [t'] -> [| $t' |]
                              ts' -> [| TsTuple $(ListE <$> sequence ts') |]

          makeRecordE :: [Q Exp] -> Q Exp {- [Q (Text, TsContext TsType)] -> Q (TsContext TsType) -}
          makeRecordE ts = if (unwrapUnaryRecords opts) && length ts == 1
                           then [| snd $(head ts) |]
                           else [| TsObject $(ListE <$> sequence ts) |]

          isNullaryCons :: ConstructorInfo -> Bool
          isNullaryCons = (\x -> length x == 0) . constructorFields 

          mkNullaryStringConsE :: ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkNullaryStringConsE c = [| TsStringLiteral $(mkConStringE . constructorName $ c) |]

          mkTaggedTypeE :: (Map Type ExpQ, Map Type Name) -> ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkTaggedTypeE m c = let conE = [| TsStringLiteral $(mkConStringE $ constructorName c) |]
                               in case sumEncoding opts of
                                      (TaggedObject tn cn) -> case constructorVariant c of
                                                                  NormalConstructor -> case constructorFields c of
                                                                                      [] -> [| TsObject [($(mkTextE tn), $conE)] |]
                                                                                      _ -> [| TsObject [($(mkTextE tn), $conE), ($(mkTextE cn), $(mkTypeE m c))] |]
                                                                  RecordConstructor ns -> makeRecordE $ [| ($(mkTextE tn), TsStringLiteral $ $(mkConStringE $ constructorName c)) |] : (mkRecordFieldE m <$> zip ns (constructorFields c))
                                      UntaggedValue -> mkTypeE m c
                                      ObjectWithSingleField -> [| TsObject [($(mkConStringE $ constructorName c), $(mkTypeE m c))] |]
                                      TwoElemArray -> [| TsTuple [$conE, $(mkTypeE m c)] |]

          mkRecordFieldE :: (Map Type ExpQ, Map Type Name) -> (Name, Type) -> Q Exp {- Q (Text, TsContext TsType) -}
          mkRecordFieldE ms (n, t) = [| ($(mkFieldStringE n), $(mkVarRef ms t)) |]

          mkNormalFieldE :: (Map Type ExpQ, Map Type Name) -> Type -> Q Exp {- Q (TsContext TsType) -}
          mkNormalFieldE = mkVarRef

          mkVarRef :: (Map Type ExpQ, Map Type Name) -> Type -> Q Exp
          mkVarRef (genericMap, subtypeMap) t = case Map.lookup t genericMap of
                                                    Just t' -> t'
                                                    Nothing -> return . VarE $ subtypeMap Map.! t

          mkFieldStringE :: Name -> Q Exp {- Q String -}
          mkFieldStringE n = mkTextE . (fieldLabelModifier opts) . nameBase $ n
        
          mkConStringE :: Name -> Q Exp {- Q String -}
          mkConStringE n = mkTextE . (constructorTagModifier opts) . nameBase $ n

          mkTextE :: String -> Q Exp {- Q Text -}
          mkTextE s = do
              os <- isExtEnabled OverloadedStrings
              if os then return . LitE . StringL $ s
                    else [| Text.pack $(return . LitE . StringL $ s) |]
              