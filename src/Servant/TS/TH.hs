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

import Servant.TS.Internal (TsTypeable(..), TsType(..), TsContext(..))

deriveTsJSON :: Options -> Name -> Q [Dec]
deriveTsJSON opts name = do
    ts <- deriveTsTypeable opts name
    js <- deriveJSON opts name
    return (ts ++ js) 

deriveTsTypeable :: Options -> Name -> Q [Dec]
deriveTsTypeable opts name = do
    TyConI (DataD _ _ _ _ cons' _) <- reify name
    [d|
        instance TsTypeable $(conT name) where
            tsTypeRep _ = $(if tagSingleConstructors opts || length cons' > 1
                            then let mk = if allNullaryToStringTag opts && all isNullaryCons cons' then mkNullaryStringConsE else mkTaggedTypeE
                                  in [| TsUnion <$> sequence $(ListE <$> sequence (mk <$> cons')) |]
                            else mkTypeE (head cons'))
     |]
    where mkTypeE :: Con -> Q Exp {- Q (TsContext TsType) -}
          mkTypeE (NormalC _ ts) = makeTupleE (mkNormalFieldE <$> ts)
          mkTypeE (RecC _ ts) = makeRecordE (mkRecordFieldE <$> ts)

          makeTupleE :: [Q Exp] -> Q Exp {- [Q TsContext TsType] -> Q (TsContext TsType) -}
          makeTupleE ts = case ts of
                              [t'] -> [| $t' |]
                              ts' -> [| TsTuple <$> sequence $(ListE <$> sequence ts') |]

          makeRecordE :: [Q Exp] -> Q Exp {- [Q (Text, TsContext TsType)] -> Q (TsContext TsType) -}
          makeRecordE ts = if (unwrapUnaryRecords opts) && length ts == 1 
                           then [| snd $(head ts) |]
                           else [| TsObject <$> mapM (\(n, TsContext t m) -> TsContext (n, t) m) $(ListE <$> sequence ts) |]

          isNullaryCons :: Con -> Bool
          isNullaryCons (NormalC _ []) = True
          isNullaryCons _ = False

          mkNullaryStringConsE :: Con -> Q Exp
          mkNullaryStringConsE (NormalC n []) = [| return . TsStringLiteral $ $(mkConStringE n) |]

          mkTaggedTypeE :: Con -> Q Exp {- Q (TsContext TsType) -}
          mkTaggedTypeE c = let conE = [| TsStringLiteral $(mkConStringE $ getConName c) |]
                             in case sumEncoding opts of
                                    (TaggedObject tn cn) -> case c of
                                                                (NormalC n ts) -> case ts of
                                                                                    [] -> [| pure $ TsObject [($(mkTextE tn), $conE)] |]
                                                                                    _ -> [| TsObject <$> sequence [pure ($(mkTextE tn), $conE), ((,) $(mkTextE cn) <$> $(mkTypeE c))] |]
                                                                (RecC n ts) -> makeRecordE $ [| ($(mkTextE tn), return . TsStringLiteral $ $(mkConStringE n)) |] : (mkRecordFieldE <$> ts)
                                    UntaggedValue -> mkTypeE c
                                    ObjectWithSingleField -> [| (\x -> TsObject [($(mkConStringE $ getConName c), x)]) <$> $(mkTypeE c) |]
                                    TwoElemArray -> [| (\x -> TsTuple [$conE, x]) <$> $(mkTypeE c) |]

          getConName :: Con -> Name
          getConName (NormalC n _) = n
          getConName (RecC n _) = n

          mkRecordFieldE :: (Name, Bang, Type) -> Q Exp {- Q (Text, TsContext TsType) -}
          mkRecordFieldE (n, _, t) = [| ($(mkFieldStringE n), tsTypeRep (Proxy :: Proxy $(return t))) |]

          mkNormalFieldE :: (Bang, Type) -> Q Exp 
          mkNormalFieldE (_, t) = [| tsTypeRep (Proxy :: Proxy $(return t)) |]

          mkFieldStringE :: Name -> Q Exp {- Q String -}
          mkFieldStringE n = mkTextE . (fieldLabelModifier opts) . nameBase $ n
        
          mkConStringE :: Name -> Q Exp {- Q String -}
          mkConStringE n = mkTextE . (constructorTagModifier opts) . nameBase $ n

          mkTextE :: String -> Q Exp {- Q Text -}
          mkTextE s = [| Text.pack $(return . LitE . StringL $ s) |]