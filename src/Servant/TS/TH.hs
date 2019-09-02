{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{- Temporary -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.TS.TH (
    deriveTsJSON,
    deriveTsTypeable
) where

import Data.Aeson.TH (Options(..), SumEncoding(..), deriveJSON)
import qualified Data.List as List
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Datatype

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Servant.TS.Core (TsType'(..), TsTypeDef(..), TsType(..), TsContext(..), ConName(..), TsTypeName(..))
import Servant.TS.Internal (TsTypeable(..), mkTsTypeName)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Monoid (Any(..))
import Data.Coerce (coerce)

deriveTsJSON :: Options -> Name -> Q [Dec]
deriveTsJSON opts name = do
    ts <- deriveTsTypeable opts name
    js <- deriveJSON opts name
    return (ts ++ js) 

{- $(stringE . show =<< reifyDatatype ''TFields) -}

{- 
    We need contraints in the generated instance for:
    * All fully saturated polymorphic types (with the exception of the type itself in the case of recursion) 
    * All polymorphic types of kind * - even it it is not used
-}

-- Cases our passed in HKT X
-- X Int                  -> X `AppT` Int
-- X Int Int              -> (X `AppT` Int) `AppT` Int
-- Either (X Int) (X Int) -> (Either `AppT` (X `AppT` Int)) `AppT` (X `AppT` Int)

-- | Returns the saturated type application of higher kinded type parameters
getHKTypeApps :: Set Type -> Type -> [Type]
getHKTypeApps ts t@(AppT l r) = if leftContains ts l 
                                then [t]
                                else getHKTypeApps ts l ++ getHKTypeApps ts r
    where leftContains :: Set Type -> Type -> Bool
          leftContains ts t@(VarT _) = Set.member t ts
          leftContains ts (AppT l _) = leftContains ts l
          leftContains _ _ = False

getHKTypeApps _ _ = []

mkConstraints :: Set Type -> [ConstructorInfo] -> [Pred]
mkConstraints hkts cons = polyFields
    where fields = concat $ constructorFields <$> cons
          polyFields = concat $ getHKTypeApps hkts <$> fields

instance Lift Text where
    lift t = mkTextE (Text.unpack t)

instance Lift ConName where
    lift (ConName p m n) = [| ConName $(lift p) $(lift m) $(lift n) |]

mkConName :: Name -> Q ConName
mkConName n = do
    loc <- location
    return $ ConName { _package = Text.pack $ loc_package loc
                     , _module  = Text.pack $ loc_module loc
                     , _name    = Text.pack $ nameBase n
                     }

isStarT :: Type -> Bool
isStarT (SigT _ StarT) = True
isStarT _ = False

mkTypeableConstraints :: [Type] -> [Pred]
mkTypeableConstraints ts = [vn | (SigT vn (AppT l r)) <- ts]

mkTopLevelTsTypeName :: Name -> [Type] -> Q Exp
mkTopLevelTsTypeName n ts = do
    con <- mkConName n
    as <- sequence $ (\v -> [| mkTsTypeName (Proxy :: Proxy $(return v)) |]) <$> ts
    [| TsTypeName $(lift con) $(return $ ListE as) |]

mkTextE :: String -> Q Exp {- Q Text -}
mkTextE s = do
    os <- isExtEnabled OverloadedStrings
    if os then return . LitE . StringL $ s
            else [| Text.pack $(return . LitE . StringL $ s) |]

deriveTsTypeable :: Options -> Name -> Q [Dec]
deriveTsTypeable opts name = do
    DatatypeInfo { datatypeInstTypes = vars
                 , datatypeVars = dVars
                 , datatypeCons = cons } <- reifyDatatype name
    
    c <- mkConName name
    -- q <- mkTsTypeName name vars 

    -- starArgs can be represented as generics in TypeScript
    let p = List.partition isStarT vars
    let starArgs = [ v | (SigT v _) <- fst p ]
    let otherArgs = [ v | (SigT v _) <- snd p ]

    stvs <- isExtEnabled ScopedTypeVariables
    _ <- if not stvs && length vars > 0 
         then fail $ "You must have the " ++ show ScopedTypeVariables ++ " language extension enabled to derive TsTypeable for polymorphic type " ++ (nameBase name) ++ "." 
         else return ()

    ui <- isExtEnabled UndecidableInstances
    _ <- if not ui && length otherArgs > 0
         then fail $ "You must have the " ++ show UndecidableInstances ++ " language extension enabled to derive TsTypable for types with higher kinded type arguments"
         else return ()
    
    {- We need to put TsTypeable constraints on any polymorphic types or applications of higher kinded polymorphics, as well as all polymorphic types -}
    let constraints = Set.fromList $ mkConstraints (Set.fromList otherArgs) cons ++ [t | (SigT t StarT) <- vars]
    mkInstanceD vars name constraints (mkTsTypeRep vars otherArgs cons)

    where mkInstanceD :: [Type] -> Name -> Set Pred -> Q Dec -> Q [Dec] {- Q  instance (TsTypeable a, ...) => TsTypable x where  -}
          mkInstanceD ts n ps d = do
              con <- conT n
              n' <- [t| TsTypeable $(return $ foldl AppT (ConT n) ((\(SigT v _) -> v) <$> ts) ) |]
              d' <- d
              let ts' = classPred ''TsTypeable . (:[]) <$> (Set.toList ps)
              let typeables = classPred ''Typeable . (:[]) <$> (mkTypeableConstraints ts)
              return [InstanceD Nothing (ts' ++ typeables) n' [d']]
                
          {- Collects all subtypes in a data declaration and generates one unqiue var name for each -}
          collectSubtypes :: Map Type ExpQ -> [ConstructorInfo] -> Q (Map Type Name)
          collectSubtypes m cs = let subTypes = concat (constructorFields <$> cs)
                                     filtered = filter (\x -> True {- not $ Map.member x m -}) subTypes
                                  in Map.fromList <$> (sequence $ (\t -> (t,) <$> newName "ts") <$> filtered)
          
          mkTsTypeRep :: [Type] -> [Type] -> [ConstructorInfo] -> Q Dec
          mkTsTypeRep vars otherArgs cons = do
              let generics = [t | (SigT t StarT) <- vars] {- We can only represent fully saturated polymorphic type arguments in TypeScript -}
              let genericArgs = Map.fromList $ (\x -> (snd x, [| TsGenericArg $(return . LitE . IntegerL . fst $ x) |])) <$> zip [0..] ((\(SigT v _) -> v) <$> vars) 
              ts <- collectSubtypes genericArgs cons
              let body = [| $(if tagSingleConstructors opts || length cons > 1
                              then let mk = if allNullaryToStringTag opts && all isNullaryCons cons then mkNullaryStringConsE else mkTaggedTypeE (genericArgs, ts)
                                    in [| TsUnion $(ListE <$> sequence (mk <$> cons)) |]
                              else mkTypeE (genericArgs, ts) (head cons))
                          |]
              
              let binds = (\(t, n) -> bindS (varP n) [| tsTypeRep (Proxy :: Proxy $(return t)) |]) <$> (Map.assocs ts)
              let context = bindS wildP [| TsContext () (Map.singleton $(mkTopLevelTsTypeName name otherArgs) $body) |]
              let ref = [| TsNamedType (TsTypeDef $(mkTopLevelTsTypeName name otherArgs) $(listE $ mkVarRef (Map.empty, Map.empty) <$> generics) $body) |]
              let dos = doE (binds ++ [context, noBindS ref])
              funD 'tsTypeRep [clause [wildP] (normalB ref) []]
            
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
                           else [| TsObject $ HashMap.fromList $(ListE <$> sequence ts) |]

          isNullaryCons :: ConstructorInfo -> Bool
          isNullaryCons = (\x -> length x == 0) . constructorFields 

          mkNullaryStringConsE :: ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkNullaryStringConsE c = [| TsStringLiteral $(mkConStringE . constructorName $ c) |]

          mkTaggedTypeE :: (Map Type ExpQ, Map Type Name) -> ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkTaggedTypeE m c = let conE = [| TsStringLiteral $(mkConStringE $ constructorName c) |]
                               in case sumEncoding opts of
                                      (TaggedObject tn cn) -> case constructorVariant c of
                                                                  NormalConstructor -> case constructorFields c of
                                                                                      [] -> [| TsObject $ HashMap.singleton $(mkTextE tn) $conE |]
                                                                                      _ -> [| TsObject $ HashMap.fromList [($(mkTextE tn), $conE), ($(mkTextE cn), $(mkTypeE m c))] |]
                                                                  RecordConstructor ns -> makeRecordE $ [| ($(mkTextE tn), TsStringLiteral $ $(mkConStringE $ constructorName c)) |] : (mkRecordFieldE m <$> zip ns (constructorFields c))
                                      UntaggedValue -> mkTypeE m c
                                      ObjectWithSingleField -> [| TsObject $ HashMap.singleton $(mkConStringE $ constructorName c) $(mkTypeE m c) |]
                                      TwoElemArray -> [| TsTuple [$conE, $(mkTypeE m c)] |]

          mkRecordFieldE :: (Map Type ExpQ, Map Type Name) -> (Name, Type) -> Q Exp {- Q (Text, TsContext TsType) -}
          mkRecordFieldE ms (n, t) = [| ($(mkFieldStringE n), $(mkVarRef ms t)) |]

          mkNormalFieldE :: (Map Type ExpQ, Map Type Name) -> Type -> Q Exp {- Q (TsContext TsType) -}
          mkNormalFieldE = mkVarRef

          mkVarRef :: (Map Type ExpQ, Map Type Name) -> Type -> Q Exp
          mkVarRef (genericMap, subtypeMap) t = [| tsTypeRep (Proxy :: Proxy $(return t)) |]

          mkFieldStringE :: Name -> Q Exp {- Q String -}
          mkFieldStringE n = mkTextE . (fieldLabelModifier opts) . nameBase $ n
        
          mkConStringE :: Name -> Q Exp {- Q String -}
          mkConStringE n = mkTextE . (constructorTagModifier opts) . nameBase $ n