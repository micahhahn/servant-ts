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
    mkInstanceD vars name (mkTsTypeRep cons)

    where mkInstanceD :: [Type] -> Name -> Q Dec -> Q [Dec] {- Q  instance (TsTypeable a, ...) => TsTypable x where  -}
          mkInstanceD ts n d = do
              con <- conT n
              n' <- [t| TsTypeable $(return $ foldl AppT (ConT n) ((\(SigT v _) -> v) <$> ts) ) |]
              d' <- d
              let ts' = (\(SigT v _) -> classPred ''TsTypeable [v]) <$> ts
              return [InstanceD Nothing ts' n' [d']]
                
          mkTsTypeRep :: [ConstructorInfo] -> Q Dec
          mkTsTypeRep cons = do
              body <- [| $(if tagSingleConstructors opts || length cons > 1
                            then let mk = if allNullaryToStringTag opts && all isNullaryCons cons then mkNullaryStringConsE else mkTaggedTypeE
                                  in [| TsUnion <$> sequence $(ListE <$> sequence (mk <$> cons)) |]
                            else mkTypeE (head cons))
                       |]
              return $ FunD ('tsTypeRep) [Clause [WildP] (NormalB body) []]
            
          mkTypeE :: ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkTypeE c = case constructorVariant c of
                          NormalConstructor -> makeTupleE (mkNormalFieldE <$> constructorFields c)
                          RecordConstructor ns -> makeRecordE (mkRecordFieldE <$> zip ns (constructorFields c))

          makeTupleE :: [Q Exp] -> Q Exp {- [Q TsContext TsType] -> Q (TsContext TsType) -}
          makeTupleE ts = case ts of
                              [t'] -> [| $t' |]
                              ts' -> [| TsTuple <$> sequence $(ListE <$> sequence ts') |]

          makeRecordE :: [Q Exp] -> Q Exp {- [Q (Text, TsContext TsType)] -> Q (TsContext TsType) -}
          makeRecordE ts = if (unwrapUnaryRecords opts) && length ts == 1 
                           then [| snd $(head ts) |]
                           else [| TsObject <$> mapM (\(n, TsContext t m) -> TsContext (n, t) m) $(ListE <$> sequence ts) |]

          isNullaryCons :: ConstructorInfo -> Bool
          isNullaryCons = (\x -> length x == 0) . constructorFields 

          mkNullaryStringConsE :: ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkNullaryStringConsE c = [| return . TsStringLiteral $ $(mkConStringE . constructorName $ c) |]

          mkTaggedTypeE :: ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkTaggedTypeE c = let conE = [| TsStringLiteral $(mkConStringE $ constructorName c) |]
                             in case sumEncoding opts of
                                    (TaggedObject tn cn) -> case constructorVariant c of
                                                                NormalConstructor -> case constructorFields c of
                                                                                    [] -> [| pure $ TsObject [($(mkTextE tn), $conE)] |]
                                                                                    _ -> [| TsObject <$> sequence [pure ($(mkTextE tn), $conE), ((,) $(mkTextE cn) <$> $(mkTypeE c))] |]
                                                                RecordConstructor ns -> makeRecordE $ [| ($(mkTextE tn), return . TsStringLiteral $ $(mkConStringE $ constructorName c)) |] : (mkRecordFieldE <$> zip ns (constructorFields c))
                                    UntaggedValue -> mkTypeE c
                                    ObjectWithSingleField -> [| (\x -> TsObject [($(mkConStringE $ constructorName c), x)]) <$> $(mkTypeE c) |]
                                    TwoElemArray -> [| (\x -> TsTuple [$conE, x]) <$> $(mkTypeE c) |]

          mkRecordFieldE :: (Name, Type) -> Q Exp {- Q (Text, TsContext TsType) -}
          mkRecordFieldE (n, t) = [| ($(mkFieldStringE n), tsTypeRep (Proxy :: Proxy $(return t))) |]

          mkNormalFieldE :: Type -> Q Exp {- Q (TsContext TsType) -}
          mkNormalFieldE t = [| tsTypeRep (Proxy :: Proxy $(return t)) |]

          mkFieldStringE :: Name -> Q Exp {- Q String -}
          mkFieldStringE n = mkTextE . (fieldLabelModifier opts) . nameBase $ n
        
          mkConStringE :: Name -> Q Exp {- Q String -}
          mkConStringE n = mkTextE . (constructorTagModifier opts) . nameBase $ n

          mkTextE :: String -> Q Exp {- Q Text -}
          mkTextE s = [| Text.pack $(return . LitE . StringL $ s) |]