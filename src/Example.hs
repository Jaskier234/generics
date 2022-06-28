{-# language TemplateHaskell #-}
module Example where

import Generics

data Prog 
    = Prog [Decl] deriving (Show, Eq)

data Decl 
    = Type String [Field] 
    | Fun String Expr deriving (Show, Eq)

data Field 
    = FInt String 
    | FData String Decl deriving (Show, Eq)

data Expr 
    = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Lit Int deriving (Show, Eq)

-- Simplifying expressions

transformExpr (Add (Lit 0) e) = e
transformExpr (Add e (Lit 0)) = e
transformExpr (Mul (Lit 1) e) = e
transformExpr (Mul e (Lit 1)) = e
transformExpr e = e

simplProg :: Prog -> Prog
simplProg (Prog ds) = Prog $ map simplDecl ds

simplDecl :: Decl -> Decl
simplDecl (Fun name body) = Fun name (simplExpr body)
simplDecl d = d

simplExpr :: Expr -> Expr
simplExpr (Add e1 e2) = transformExpr $ Add (simplExpr e1) (simplExpr e2)
simplExpr (Sub e1 e2) = transformExpr $ Sub (simplExpr e1) (simplExpr e2)
simplExpr (Mul e1 e2) = transformExpr $ Mul (simplExpr e1) (simplExpr e2)
simplExpr (Div e1 e2) = transformExpr $ Div (simplExpr e1) (simplExpr e2)
simplExpr e = e

$(everywhere1 "simplProg'" 'transformExpr [t| Prog |] [t| Expr |])

program = Prog [
    Fun "f1" (Sub (Add (Lit 0) (Add (Mul (Lit 44) (Lit 1)) (Lit 0))) (Lit 1)),
    Type "d1" [FInt "n"],
    Type "d2" [FData "r" (Type "n" [FInt "n1", FInt "n2"])],
    Fun "f2" (Lit 2)]

test1 :: IO ()
test1 = do 
  putStrLn "test1"
  putStrLn "Before transformation"
  print program
  putStrLn "After simplProg" 
  print $ simplProg program
  putStrLn "After simplProg'" 
  print $ simplProg' program


-- Mutual recursion

transformField :: Field -> Field
transformField (FInt name) = FData "n" (Type "n" [FInt name])
transformField (FData name decl) = FData ("n_" ++ name) decl

$(everywhere1 "simplTypeDecl" 'transformField [t| Prog |] [t| Field |])

test2 :: IO ()
test2 = do
  putStrLn "After simplTypeDecl"
  print $ simplTypeDecl program

exampleMain = test1 >> test2

