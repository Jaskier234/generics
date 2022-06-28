{-# language TemplateHaskell #-}
module Generics
    ( everywhere1
    , execEverywhere
    , printToExp
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Datatype
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader

import Debug.Trace ( trace )

type StateQ a = ReaderT (Map Name Type) (StateT (Map Type (Maybe Exp)) Q) a
type DecList = [Dec] -> [Dec]

execEverywhere :: String -> Name -> Q Type -> Q Type -> Q (Map Type (Maybe Exp))
execEverywhere mainFuncName transformFuncName mainType transformType = do 
  main <- mainType
  transform <- transformType
  execStateT (runReaderT (generateEverywhere mainFuncName transformFuncName main transform) Map.empty) Map.empty

everywhere1 :: String -> Name -> Q Type -> Q Type -> Q [Dec]
everywhere1 mainFuncName transformFuncName mainType transformType = do 
  main <- mainType
  transform <- transformType
  evalStateT (runReaderT (generateEverywhere mainFuncName transformFuncName main transform) Map.empty) Map.empty

generateEverywhere :: String -> Name -> Type -> Type -> StateQ [Dec]
generateEverywhere mainFuncName transformFuncName mainTypeUnresolved transformTypeUnresolved = do 
  mainType <- lift $ lift $ resolveTypeSynonyms mainTypeUnresolved
  (decls, _) <- runType mainType

  idExp <- lift $ lift [| id |]
  body <- gets $ (fromMaybe idExp) . fromJust . (Map.lookup mainType)
  let alias = FunD (mkName mainFuncName) [Clause [] (NormalB body) (decls [])]
  return $ [alias]

  where
    runType :: Type -> StateQ (DecList, Bool)
    runType beforeSubstT = do
      -- Simplify types
      varMap <- ask
      let t' = applySubstitution varMap beforeSubstT
      t <- lift $ lift $ resolveTypeSynonyms t'
      transformType <- lift $ lift $ resolveTypeSynonyms transformTypeUnresolved

      -- Check if type is in map
      isPresent <- gets $ Map.member t 
      if isPresent
        then 
          return (id, t == transformType)
        else do 
          funName <- lift $ lift $ newName "fun"
          modify $ Map.insert t (Just $ VarE funName)

          when (t == transformType) (do
            applyTransform <- lift $ lift [| \f x -> $(return $ VarE transformFuncName) (f x) |]
            let composeWithTransformation = \new old -> case old of
                          Just oldExp -> Just $ AppE applyTransform oldExp
                          Nothing  -> error $ "Old value for type " ++ show t ++ " is Nothing"
            modify $ Map.insertWith composeWithTransformation t (Nothing))

          (retDec, retUsed) <- case t of
              (ConT name) -> do
                -- Run recursively for subtypes
                typeInfo <- lift $ lift $ reify name
                (decs, used) <- runInfo typeInfo

                -- If transformType is not used in subtree then 
                -- replace expression in map with Nothing
                newDecs <- if not (used || t == transformType)
                            then do 
                              modify $ Map.insert t Nothing
                              return id
                            else do 
                              generateFunction funName name 

                return (decs . newDecs, used || t == transformType)

              (AppT t1 t2) -> do
                (decls2, used2) <- runType t2

                -- Run below in the local env where first free variable in t1 is substituted by t2
                maybeVar <- lift $ lift $ getFirstFreeVar t1
                (decls1, used1) <- case maybeVar of 
                                    Just var -> local (Map.insert var t2) $ runType t1
                                    Nothing  -> runType t1
                
                -- Check if there is transformType as subtype of t
                appDec <- if used1 || used2
                  then do
                    idExp <- lift $ lift [| id |]
                    exp1 <- gets $ (fromMaybe idExp) . fromJust . (Map.lookup t1)
                    exp2 <- gets $ (fromMaybe idExp) . fromJust . (Map.lookup t2)
                    let appExp = AppE exp1 exp2
                    let alias = \t -> (FunD funName [Clause [] (NormalB appExp) []]):t
                    return alias
                  else do 
                    modify $ Map.insert t Nothing
                    return id

                return (decls1 . decls2 . appDec, used1 || used2 || t == transformType)

              ListT -> do 
                mapExp <- lift $ lift [| map |]
                modify $ Map.insert t (Just mapExp)
                return (id, False)
              t -> do 
                modify $ Map.insert t Nothing
                return (id, False) 
        
          return (retDec, retUsed)

      where
        getFirstFreeVar :: Type -> Q (Maybe Name)
        getFirstFreeVar t = getNthVar 0 t
          where
            getNthVar n (ConT name) = do 
              dataInfo <- reifyDatatype name
              -- (!!) fail here means that ConT was applied too many times
              return $ Just $ tvName $ (datatypeVars dataInfo) !! n
            getNthVar n (AppT t1 t2) = getNthVar (n+1) t1
            getNthVar _ t = return Nothing


    runInfo :: Info -> StateQ (DecList, Bool)
    runInfo (TyConI (DataD _ name _ _ cons _)) = do 
      conDecls <- mapM runCon cons
      return $ foldl foldDecls (id, False) conDecls
    runInfo (TyConI (NewtypeD _ name _ _ con _)) = do 
      runCon con
    runInfo (TyConI (TySynD name _ t)) = do 
      runType t
    runInfo (PrimTyConI _ _ _) = return (id, False)

    runCon :: Con -> StateQ (DecList, Bool)
    runCon (NormalC name ts) = do
      tDecls <- mapM (runType . snd) ts
      return $ foldl foldDecls (id, False) tDecls
    
    foldDecls :: (DecList, Bool) -> (DecList, Bool) -> (DecList, Bool)
    foldDecls (accDecls, accUsed) (decls, used) = (accDecls . decls, accUsed || used)

    generateFunction :: Name -> Name -> StateQ DecList
    generateFunction funcName typeName = 
      do
        (TyConI dataDecl) <- lift $ lift $ reify typeName
        funDecl <- generateDeclaration dataDecl
        return $ \l -> funDecl:l

      where
        generateDeclaration :: Dec -> StateQ Dec
        generateDeclaration d = 
          case d of 
            (DataD _ name vars _ cons _)   -> genCases vars cons
            (NewtypeD _ name vars _ con _) -> genCases vars [con]
          where
            genCases :: [TyVarBndr a] -> [Con] -> StateQ Dec
            genCases vars cons = 
              do 
                cases <- mapM (generateCase vars) cons
                return $ FunD funcName cases
                

        generateCase :: [TyVarBndr a] -> Con -> StateQ Clause
        generateCase vars (NormalC conName types) = 
          do
            names <- lift $ lift $ mapM newName (map (const "x") types)  
            pat <- pattern names
            
            -- Normalize types
            varMap <- ask
            substitutedTypes <- lift $ lift $ mapM resolveTypeSynonyms (applySubstitution varMap $ map snd types)
            exp <- body names substitutedTypes

            let typeVarArgs = map (\v -> VarP $ tvName v) vars
            return $ Clause (typeVarArgs ++ [pat]) (NormalB exp) []

          where
            pattern varNames = do 
              return $ ConP conName (map VarP varNames)

            body varNames types = do
              foldM apply (ConE conName) (zip varNames types)

            apply :: Exp -> (Name, Type) -> StateQ Exp
            apply exp (var, t) = do 
              varFunc <- gets $ fromJust . (Map.lookup t)
              case varFunc of
                Just func -> return $ AppE exp (AppE func (VarE var))
                Nothing   -> return $ AppE exp (VarE var)
        
printToExp :: Show a => a -> Q Exp
printToExp a = stringE $ show a

