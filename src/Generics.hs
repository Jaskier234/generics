{-# language TemplateHaskell #-}
module Generics
    ( evalEverywhere
    , execEverywhere
    -- , printToExp
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State

import Debug.Trace ( trace )

type StateQ a = StateT (Map Type (Maybe Exp)) Q a
type DecList = [Dec] -> [Dec]

execEverywhere :: String -> Name -> Q Type -> Q Type -> Q (Map Type (Maybe Exp))
execEverywhere mainFuncName transformFuncName mainType transformType = do 
  main <- mainType
  transform <- transformType
  execStateT (generateEverywhere mainFuncName transformFuncName main transform) Map.empty

evalEverywhere :: String -> Name -> Q Type -> Q Type -> Q [Dec]
evalEverywhere mainFuncName transformFuncName mainType transformType = do 
  main <- mainType
  transform <- transformType
  evalStateT (generateEverywhere mainFuncName transformFuncName main transform) Map.empty

generateEverywhere :: String -> Name -> Type -> Type -> StateQ [Dec]
generateEverywhere mainFuncName transformFuncName mainType transformType = do 
  (decls, _) <- runType mainType
  idExp <- lift [| id |]
  body <- gets $ (fromMaybe idExp) . fromJust . (Map.lookup mainType)
  let alias = FunD (mkName mainFuncName) [Clause [] (NormalB body) (decls [])]
  return $ [alias]

  where
    runType :: Type -> StateQ (DecList, Bool)
    runType t = do
      -- Check if type is in map
      isPresent <- gets $ Map.member t 
      if isPresent
        then 
          return (id, t == transformType)
        else case t of
              (ConT name) -> do
                -- Add new function name to map
                funName <- lift $ newName $ "fun" ++ nameBase name
                modify $ Map.insert t (Just $ VarE funName)

                -- Run recursively for subtypes
                typeInfo <- lift $ reify name
                (decs, used) <- runInfo typeInfo

                -- If transformType is not used in subtree then 
                -- replace expression in map with Nothing
                newDecs <- if not (used || t == transformType)
                            then do 
                              modify $ Map.insert t Nothing
                              return id
                            else do 
                              transformFunc <- if t == transformType
                                then return $ Just $ VarE transformFuncName
                                else return Nothing
                              generateFunction funName name transformFunc

                return (decs . newDecs, used || t == transformType)

              (AppT t1 t2) -> do
                -- Insert Nothing to indicate that t is already visited
                modify $ Map.insert t Nothing

                (decls1, used1) <- runType t1
                (decls2, used2) <- runType t2
                
                -- Check if there is transformType as subtype of t
                if used1 || used2
                  then do
                    -- Insert proper Exp
                    idExp <- lift [| id |]
                    exp1 <- gets $ (fromMaybe idExp) . fromJust . (Map.lookup t1)
                    exp2 <- gets $ (fromMaybe idExp) . fromJust . (Map.lookup t2)
                    let appExp = AppE exp1 exp2
                    modify $ Map.insert t (Just appExp)
                  else return ()

                return (decls1 . decls2, used1 || used2 || t == transformType)

              ListT -> do 
                mapExp <- lift [| map |]
                modify $ Map.insert t (Just mapExp)
                return (id, False)
              t -> do 
                modify $ Map.insert t Nothing
                return (id, False) 

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

    generateFunction :: Name -> Name -> Maybe Exp -> StateQ DecList
    generateFunction funcName typeName transformFunc = 
      do
        (TyConI dataDecl) <- lift $ reify typeName
        funDecl <- generateDeclaration dataDecl
        return $ \l -> funDecl:l

      where
        generateDeclaration :: Dec -> StateQ Dec
        generateDeclaration (DataD _ name _ _ cons _) = 
          do 
            cases <- mapM generateCase cons
            return $ FunD funcName cases

        generateCase :: Con -> StateQ Clause
        generateCase (NormalC conName types) = 
          do
            names <- lift $ mapM newName (map (const "x") types)  
            pat <- pattern names
            exp <- body names
            return $ Clause [pat] (NormalB exp) []

          where
            pattern varNames = do 
              return $ ConP conName (map VarP varNames)

            body varNames = do
              caseBody <- foldM apply (ConE conName) (zip varNames (map snd types))
              case transformFunc of
                Just func -> return $ AppE func caseBody
                Nothing   -> return caseBody

            apply :: Exp -> (Name, Type) -> StateQ Exp
            apply exp (var, t) = do 
              varFunc <- gets $ fromJust . (Map.lookup t)
              case varFunc of
                Just func -> return $ AppE exp (AppE func (VarE var))
                Nothing   -> return $ AppE exp (VarE var)
        
printToExp :: Show a => a -> Q Exp
printToExp a = stringE $ show a

