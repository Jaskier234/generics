{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module DataGenerics where

import Data.Generics.Schemes
import Data.Generics.Aliases
import Data.Data
import Generics

data Color = R | G | B deriving (Show, Data)

data Expr
  = Add Expr Expr
  | Mul Expr Expr 
  | Col Color
  | Lit Int deriving (Show, Data)

data Decl
  = Fun String Expr
  | Type String [Int] deriving (Show, Data)

data Prog = Prog [Decl] deriving (Show, Data)

prog = Prog
  [ Fun "f1" (Lit 0) 
  , Type "t1" [1,2,3]
  , Fun "f2" (Add (Lit 0) (Add (Lit 42) (Lit 0))) ]

simplAdd :: Expr -> Expr
simplAdd (Add (Lit 0) l) = l
simplAdd (Add l (Lit 0)) = l
simplAdd exp = exp

-- I want to generate such function:
simplProg :: Prog -> Prog
simplProg (Prog p) = Prog (map simplDecl p)

simplDecl :: Decl -> Decl
simplDecl (Fun name body) = Fun name (simplExpr body)
simplDecl (Type name ints) = Type name ints

simplExpr :: Expr -> Expr
simplExpr (Add e1 e2) = simplAdd $ Add (simplExpr e1) (simplExpr e2)
simplExpr (Mul e1 e2) = simplAdd $ Mul (simplExpr e1) (simplExpr e2)
simplExpr (Lit int) = simplAdd $ Lit int


simplProg2 :: Prog -> Prog
simplProg2 = everywhere $ mkT simplAdd

-- Potential problems with (mutually) recursive data types

-- How it works with type variables 

-- What if top-level type is equal to function type (for example
-- we run tranformation on whole program)

-- Test running time

-- $(evalEverywhere 'succ [t| Prog |] [t| Int |])


-- simplProg3 = funProg

-- test: mutual recursion

data D = D Int E | DL Int deriving Show

data E = E Char D | EL Char deriving Show

d = D 34 $ E 'e' $ D 44 $ E 'g' $ D 234 $ E 'd' $ DL 33

dFun (D n e) = D 12 e
dFun (DL n) = D 44 (EL 'e')

$(evalEverywhere 'dFun [t| D |] [t| D |])

testMain :: IO ()
testMain = do 
    putStrLn "Before tranformation" 
    print d
    putStrLn "After tranformation" 
    print $ funD d

-- test: bigger tests
-- test: what with type variables
-- TODO better names, put decls in where clause to avoid collisions
-- TODO general cleanup in Generics.hs
