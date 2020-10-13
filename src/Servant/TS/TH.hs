{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

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

deriveTsJSON :: Options -> Name -> Q [Dec]
deriveTsJSON opts name = do
    ts <- deriveTsTypeable opts name
    js <- deriveJSON opts name
    return (ts ++ js) 

mkConstraints :: Set Type -> [ConstructorInfo] -> [Pred]
mkConstraints hkts cons = polyFields
    where fields = concat $ constructorFields <$> cons
          polyFields = filter (containsHKT hkts) $ fields

containsHKT :: Set Type -> Type -> Bool
containsHKT hkts = f
    where f (ForallT _ _ t) = f t
          f (AppT l r) = f l || f r
          f (SigT l r) = f l || f r
          f (VarT x) =  Set.member (VarT x) hkts
          f _ = False

instance Lift ConName where
    lift (ConName p m n) = [| ConName |] `appE` (lift p) `appE` (lift m) `appE` (lift n)

mkTextE :: String -> Q Exp {- Q Text -}
mkTextE s = do
    os <- isExtEnabled OverloadedStrings
    if os then return . LitE . StringL $ s
          else [| Text.pack |] `appE` (litE . StringL $ s)

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
                  in TsTypeName cons (mkTsTypeName <$> args) 0

mkProxy :: Type -> ExpQ
mkProxy t = [| Proxy |] `sigE` ([t| Proxy |] `appT` (return t))

mkTopLevelTsTypeName :: Name -> [Type] -> Q Exp
mkTopLevelTsTypeName n ts = do
    con <- mkConName n
    let as = [[| typeRep |] `appE` mkProxy v | SigT v (AppT _ _) <- ts ]
    let hkts = ([| mkTsTypeName |] `appE`) <$> as
    [| TsTypeName |] `appE` lift con `appE` listE hkts `appE` (litE . IntegerL . fromIntegral $ (length ts - length as))

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
              n' <- [t| TsTypeable |] `appT` (return $ foldl AppT (ConT n) ((\(SigT v _) -> v) <$> ts)) 
              d' <- d
              let ts' = classPred ''TsTypeable . (:[]) <$> (Set.toList ps)
              let typeables = classPred ''Typeable . (:[]) <$> (mkTypeableConstraints ts)
              return [InstanceD Nothing (ts' ++ typeables) n' [d']]
          
          mkTsTypeRep :: [Type] -> [Type] -> [ConstructorInfo] -> Q Dec
          mkTsTypeRep starArgs allArgs cons = do
              let genericArgs = Map.fromList $ (\x -> (snd x, [| TsGenericArg |] `appE` (lift . fst $ x))) <$> (zip ([0..] :: [Int]) starArgs) 
              let body = if tagSingleConstructors opts || length cons > 1
                         then let mk = if allNullaryToStringTag opts && all isNullaryCons cons then mkNullaryStringConsE else mkTaggedTypeE genericArgs
                               in [| TsUnion |] `appE` listE (mk <$> cons)
                         else mkTypeE genericArgs (head cons)
              
              let gs = ListE <$> mapM (mkVarRef Map.empty) starArgs
              let ref = [| TsNamedType |] `appE` (mkTopLevelTsTypeName name allArgs) `appE` gs `appE` ([| TsDef |] `appE` body)
              funD 'tsTypeRep [clause [wildP] (normalB ref) []]
            
          mkTypeE :: (Map Type ExpQ) -> ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkTypeE m c = case constructorVariant c of
                            NormalConstructor -> makeTupleE (mkNormalFieldE m <$> constructorFields c)
                            RecordConstructor ns -> makeRecordE (mkRecordFieldE m <$> zip ns (constructorFields c))

          makeTupleE :: [Q Exp] -> Q Exp {- [Q TsContext TsType] -> Q (TsContext TsType) -}
          makeTupleE ts = case ts of
                              [t'] -> t'
                              ts' -> [| TsTuple |] `appE` listE ts'

          makeRecordE :: [Q Exp] -> Q Exp {- [Q (Text, TsContext TsType)] -> Q (TsContext TsType) -}
          makeRecordE ts = if (unwrapUnaryRecords opts) && length ts == 1
                           then [| snd |] `appE` head ts
                           else [| TsObject |] `appE` ([| HashMap.fromList |] `appE` listE ts)

          isNullaryCons :: ConstructorInfo -> Bool
          isNullaryCons = null . constructorFields 

          mkNullaryStringConsE :: ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkNullaryStringConsE c = [| TsStringLiteral |] `appE` (mkConStringE . constructorName $ c)

          mkTaggedTypeE :: Map Type ExpQ -> ConstructorInfo -> Q Exp {- Q (TsContext TsType) -}
          mkTaggedTypeE m c = let conE = [| TsStringLiteral |] `appE` (mkConStringE . constructorName $ c)
                               in case sumEncoding opts of
                                      (TaggedObject tn cn) -> case constructorVariant c of
                                                                  NormalConstructor -> case constructorFields c of
                                                                                      [] -> [| TsObject |] `appE` ([| HashMap.singleton |] `appE` mkTextE tn `appE` conE)
                                                                                      _ -> let item1 = [| (,) |] `appE` mkTextE tn `appE` conE
                                                                                               item2 = [| (,) |] `appE` mkTextE cn `appE` mkTypeE m c
                                                                                            in [| TsObject |] `appE` ([| HashMap.fromList |] `appE` listE [item1, item2])
                                                                  RecordConstructor ns -> let head' = [| (,) |] `appE` mkTextE tn `appE` ([| TsStringLiteral |] `appE` (mkConStringE . constructorName $ c))
                                                                                              tail' = mkRecordFieldE m <$> zip ns (constructorFields c)
                                                                                           in makeRecordE (head' : tail')
                                      UntaggedValue -> mkTypeE m c
                                      ObjectWithSingleField -> [| TsObject |] `appE` ([| HashMap.singleton |] `appE` (mkConStringE . constructorName $ c) `appE` (mkTypeE m c))
                                      TwoElemArray -> [| TsTuple |] `appE` listE [conE, mkTypeE m c]

          mkRecordFieldE :: (Map Type ExpQ) -> (Name, Type) -> Q Exp {- Q (Text, TsContext TsType) -}
          mkRecordFieldE m (n, t) = [| (,) |] `appE` mkFieldStringE n `appE` mkVarRef m t

          mkNormalFieldE :: (Map Type ExpQ) -> Type -> Q Exp {- Q (TsContext TsType) -}
          mkNormalFieldE = mkVarRef

          mkVarRef :: (Map Type ExpQ) -> Type -> Q Exp
          mkVarRef m t' = case Map.lookup t' m of
                              Just e -> e 
                              Nothing -> let gs' = getGenerics m t'
                                          in if null gs' then [| tsTypeRep |] `appE` mkProxy t'
                                                         else let match1 = do n <- newName "n"
                                                                              gs <- newName "gs"
                                                                              t <- newName "t"
                                                                              let body = [| TsNamedType |] `appE` (varE n) `appE` ([| zipF |] `appE` (listE . getGenerics m $ t') `appE` varE gs) `appE` (varE t)
                                                                              match (conP 'TsNamedType [varP n, varP gs, varP t]) (normalB $ body) []
                                                                  match2 = do n <- newName "x" 
                                                                              match (varP n) (normalB . varE $ n ) []
                                                               in caseE ([| tsTypeRep |] `appE` mkProxy t') [ match1, match2 ]

          getGenerics :: (Map Type ExpQ) -> Type -> [ExpQ] {- [Q (TsType -> TsType)] -} 
          getGenerics gs (AppT l r) = (f l ++ f r)
            where f (AppT l r)  = f l ++ f r
                  f t = case Map.lookup t gs of 
                            Just t' -> [ [| const |] `appE` t' ]
                            Nothing -> [ [| id |] ]
          getGenerics _ _ = []

          mkFieldStringE :: Name -> Q Exp {- Q String -}
          mkFieldStringE = mkTextE . fieldLabelModifier opts . nameBase
        
          mkConStringE :: Name -> Q Exp {- Q String -}
          mkConStringE = mkTextE . constructorTagModifier opts . nameBase
