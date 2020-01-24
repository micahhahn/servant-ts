{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.TS.TH (
    deriveTsJSON,
    deriveTsTypeable
) where

import Control.Monad (when)
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

import Servant.TS.Core
import Servant.TS.Internal (TsTypeable(..))

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Monoid (Any(..))
import Data.Coerce (coerce)

makeBaseFunctor ''Type

deriveTsJSON :: Options -> Name -> Q [Dec]
deriveTsJSON opts name = do
    ts <- deriveTsTypeable opts name
    js <- deriveJSON opts name
    return (ts ++ js) 

{- TODO: Use reifyInstances -}

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
          polyFields = filter (containsHKT hkts) $ fields

containsHKT :: Set Type -> Type -> Bool
-- containsHKT hkts t = coerce $ foldMap (Any . flip Set.member hkts) (project t)
containsHKT hkts t = cata f t
    where f (VarTF x) = Set.member (VarT x) hkts
          f t' = coerce $ foldMap Any t'

deriving instance Show a => Show (TypeF a)

instance Lift Text where
    lift t = mkTextE (Text.unpack t)

instance Lift ConName where
    lift (ConName p m n) = [| ConName $(lift p) $(lift m) $(lift n) |]

mkTextE :: String -> Q Exp {- Q Text -}
mkTextE s = do
    os <- isExtEnabled OverloadedStrings
    if os then return . LitE . StringL $ s
            else [| Text.pack $(return . LitE . StringL $ s) |]

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

mkTsTypeName :: TypeRep -> TsTypeName
mkTsTypeName t = let (con, args) = splitTyConApp t
                     mk f = Text.pack . f $ con
                     cons =  ConName (mk tyConPackage) (mk tyConModule) (mk tyConName)
                  in TsTypeName cons (TsHKT . mkTsTypeName <$> args)

mkTopLevelTsTypeName :: Name -> [Type] -> Q Exp
mkTopLevelTsTypeName n ts = do
    con <- mkConName n
    as <- sequence $ (\t -> case t of
                                (SigT (VarT n) StarT) -> [| TsGeneric $(mkTextE . nameBase $ n) |]
                                (SigT v _) -> [| TsHKT (mkTsTypeName (typeRep (Proxy :: Proxy $(return v)))) |]) <$> ts
    [| TsTypeName $(lift con) $(return $ ListE as) |]

zipF :: [a -> b] -> [a] -> [b]
zipF fs as = (\(f, a) -> f a) <$> zip fs as

deriveTsTypeable :: Options -> Name -> Q [Dec]
deriveTsTypeable opts name = do
    DatatypeInfo { datatypeInstTypes = vars
                 , datatypeCons = cons } <- reifyDatatype name
    
    -- starArgs can be represented as generics in TypeScript
    let p = List.partition isStarT vars
    let starArgs = [ v | (SigT v _) <- fst p ]
    let otherArgs = [ v | (SigT v _) <- snd p ]

    -- Show error if our type has arguments and ScopedTypeVariables is not enabled
    stvs <- isExtEnabled ScopedTypeVariables
    when (not stvs && not (null vars)) (fail $ "You must have the " ++ show ScopedTypeVariables ++ " language extension enabled to derive TsTypeable for polymorphic type " ++ nameBase name ++ ".")

    -- Show error if our type has higher kinded arguments and UndecidableInstances and MonoLocalBinds are not enabled
    ui <- isExtEnabled UndecidableInstances
    mlb <- isExtEnabled MonoLocalBinds
    when ((not ui || not mlb) && not (null otherArgs)) (fail $ "You must have the " ++ show UndecidableInstances ++ " and " ++ show MonoLocalBinds ++ " language extensions enabled to derive TsTypeable for types with arguments of kind (k -> *)")
    
    {- We need to put TsTypeable constraints on any polymorphic types or applications of higher kinded polymorphics, as well as all polymorphic types -}
    let constraints = Set.fromList $ mkConstraints (Set.fromList otherArgs) cons ++ starArgs
    mkInstanceD vars name constraints (mkTsTypeRep starArgs vars cons)

    where mkInstanceD :: [Type] -> Name -> Set Pred -> Q Dec -> Q [Dec] {- Q  instance (TsTypeable a, ...) => TsTypable x where  -}
          mkInstanceD ts n ps d = do
              con <- conT n
              n' <- [t| TsTypeable $(return $ foldl AppT (ConT n) ((\(SigT v _) -> v) <$> ts) ) |]
              d' <- d
              let ts' = classPred ''TsTypeable . (:[]) <$> (Set.toList ps)
              let typeables = classPred ''Typeable . (:[]) <$> (mkTypeableConstraints ts)
              return [InstanceD Nothing (ts' ++ typeables) n' [d']]
          
          mkTsTypeRep :: [Type] -> [Type] -> [ConstructorInfo] -> Q Dec
          mkTsTypeRep starArgs allArgs cons = do
              let genericArgs = Map.fromList $ (\x -> (snd x, [| TsGenericArg $(lift . fst $ x) |])) <$> (zip ([0..] :: [Int]) starArgs) 
              let body = [| $(if tagSingleConstructors opts || length cons > 1
                              then let mk = if allNullaryToStringTag opts && all isNullaryCons cons then mkNullaryStringConsE else mkTaggedTypeE genericArgs
                                    in [| TsUnion $(ListE <$> sequence (mk <$> cons)) |]
                              else mkTypeE genericArgs (head cons))
                          |]
              
              let gs = ListE <$> mapM (mkVarRef Map.empty) starArgs
              let ref = [| TsNamedType $(mkTopLevelTsTypeName name allArgs) $gs (TsDef $body) |]
              funD 'tsTypeRep [clause [wildP] (normalB ref) []]
            
          mkTypeE :: (Map Type ExpQ) -> ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
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

          mkTaggedTypeE :: (Map Type ExpQ) -> ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
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

          mkRecordFieldE :: (Map Type ExpQ) -> (Name, Type) -> Q Exp {- Q (Text, TsContext TsType) -}
          mkRecordFieldE m (n, t) = [| ($(mkFieldStringE n), $(mkVarRef m t)) |]

          mkNormalFieldE :: (Map Type ExpQ) -> Type -> Q Exp {- Q (TsContext TsType) -}
          mkNormalFieldE = mkVarRef

          mkVarRef :: (Map Type ExpQ) -> Type -> Q Exp
          mkVarRef m t' = case Map.lookup t' m of
                              Just e -> e 
                              Nothing -> let gs' = getGenerics m t'
                                          in if null gs' then [| tsTypeRep (Proxy :: Proxy $(return t')) |]
                                                         else [| case tsTypeRep (Proxy :: Proxy $(return t')) of 
                                                                    TsNamedType n gs t -> TsNamedType n (zipF ($(listE . getGenerics m $ t')) gs) t
                                                                    x -> x|]

          getGenerics :: (Map Type ExpQ) -> Type -> [ExpQ] {- [Q (TsType -> TsType)] -} 
          getGenerics gs (AppT l r) = (f l ++ f r)
            where f (AppT l r)  = f l ++ f r
                  f t = case Map.lookup t gs of 
                            Just t' -> [ [| const $t' |] ]
                            Nothing -> [ [| id |] ]
          getGenerics _ _ = []

          mkFieldStringE :: Name -> Q Exp {- Q String -}
          mkFieldStringE = mkTextE . (fieldLabelModifier opts) . nameBase
        
          mkConStringE :: Name -> Q Exp {- Q String -}
          mkConStringE = mkTextE . (constructorTagModifier opts) . nameBase