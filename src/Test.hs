{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module Test where

import Data.Generics.Schemes
import Data.Generics.Aliases
import Data.Data
import Generics
import Language.Haskell.TH ( mkName, runQ, reify)
import Language.Haskell.TH.Datatype
import Data.Map ( Map )
import qualified Data.Map as Map

import qualified Func.Abs as Abs
import Func.Par

data Color = R | G | B deriving (Show, Data, Eq)

data Expr
  = Add Expr Expr
  | Mul Expr Expr 
  | Col Color
  | Lit Int deriving (Show, Data, Eq)

data Decl
  = Fun String Expr
  | Type String [Int] deriving (Show, Data, Eq)

data Prog = Prog [Decl] deriving (Show, Data, Eq)

testEq :: (Eq a, Show a) => String -> a -> a -> IO ()
testEq name e1 e2 = do 
  if e1 == e2 
    then putStrLn $ "test " ++ name ++ " Passed"
    else do 
      putStrLn $ "test " ++ name ++ " Failed" 
      putStrLn $ "e1: " ++ show e1
      putStrLn $ "e2: " ++ show e2

prog = Prog
  [ Fun "f1" (Lit 0) 
  , Type "t1" [1,2,3]
  , Fun "f2" (Add (Lit 0) (Add (Lit 42) (Lit 0))) ]

simplAdd :: Expr -> Expr
simplAdd (Add (Lit 0) l) = l
simplAdd (Add l (Lit 0)) = l
simplAdd exp = exp


-- -- I want to generate such function:
-- simplProg :: Prog -> Prog
-- simplProg (Prog p) = Prog (map simplDecl p)
-- 
-- simplDecl :: Decl -> Decl
-- simplDecl (Fun name body) = Fun name (simplExpr body)
-- simplDecl (Type name ints) = Type name ints
-- 
-- simplExpr :: Expr -> Expr
-- simplExpr (Add e1 e2) = simplAdd $ Add (simplExpr e1) (simplExpr e2)
-- simplExpr (Mul e1 e2) = simplAdd $ Mul (simplExpr e1) (simplExpr e2)
-- simplExpr (Lit int) = simplAdd $ Lit int
-- 
-- 
-- simplProg2 :: Prog -> Prog
-- simplProg2 = everywhere $ mkT simplAdd
-- 
-- -- How it works with type variables 
-- 
-- -- Test running time
-- 

$(everywhere1 "tProgSimplAdd" 'simplAdd [t| Prog |] [t| Expr |])

test1_1 = testEq "test 1.1" (tProgSimplAdd prog) (Prog
  [ Fun "f1" (Lit 0) 
  , Type "t1" [1,2,3]
  , Fun "f2" (Lit 42) ])

$(everywhere1 "tProgSucc" 'succ [t| Prog |] [t| Int |])

test1_2 = testEq "test 1.2" (tProgSucc prog) (Prog
  [ Fun "f1" (Lit 1) 
  , Type "t1" [2,3,4]
  , Fun "f2" (Add (Lit 1) (Add (Lit 43) (Lit 1))) ])

-- 
-- 
-- simplProg3 = tProg

-- test: mutual recursion

data D a = D Int (E a) | DL Int deriving (Show, Eq)

data E a = E Char (D a) | EL Char deriving (Show, Eq)

d :: D Int
d = D 34 $ E 'e' $ D 44 $ E 'g' $ D 234 $ E 'd' $ DL 33

dFun (D n e) = D 12 e
dFun (DL n) = D 44 (EL 'e')

$(everywhere1 "funD" 'dFun [t| D Int |] [t| D Int |])

test2_1 = testEq "test 2.1" (funD d) (D 12 $ E 'e' $ D 12 $ E 'g' $ D 12 $ E 'd' $ D 44 $ EL 'e')

-- test: what with type variables 
data P a = P [De a] a deriving (Show, Eq, Data)
data De a = F String (Ex a) deriving (Show, Eq, Data)
data Ex a = Ad (Ex a) (Ex a) | Li Integer | A a deriving (Show, Eq, Data)

pp1 = P [F "ff" (Ad (Li 42) (Li 0))] 3

funPP1 (Ad e (Li 0)) = e
funPP1 e = e

$(everywhere1 "funP" 'funPP1 [t| P Int |] [t| Ex Int |])

test3_1 = testEq "test 3.1" (funP pp1) (P [F "ff" (Li 42)] 3)


-- test type variables
data T1 a = C a deriving (Show, Eq, Data)

t1 :: T1 Integer
t1 = C 12

-- transformT1 = everywhere $ mkT (succ :: Integer -> Integer)
$(everywhere1 "transformT1" 'succ [t| T1 Integer |] [t| Integer |])

test4_1 = testEq "test 4.1" (transformT1 t1) (C 13)

data T2 a b = T2 a b deriving (Show, Eq, Data)

t2 :: T2 Integer Char
t2 = T2 33 'a'

$(everywhere1 "transformT2Int" 'succ [t| T2 Integer Char |] [t| Integer |])

constChar = const 's'
$(everywhere1 "transformT2Char" 'constChar [t| T2 Integer Char |] [t| Char |])

test4_2 = testEq "test 4.2" (transformT2Int t2) (T2 34 'a') 
test4_3 = testEq "test 4.3" (transformT2Char t2) (T2 33 's')

-- test: bigger tests

program =      (\(Right a) -> a) $ pProgram . myLexer $ " type Int -> Int def f n = (m+0)+0+n"
programSimpl = (\(Right a) -> a) $ pProgram . myLexer $ " type Int -> Int def f n = (m)    +n"

simplAdd' :: Abs.Exp -> Abs.Exp
simplAdd' (Abs.EAdd p e (Abs.ENum _ 0)) = e
simplAdd' e = e

$(everywhere1 "simplProg" 'simplAdd' [t| Abs.Program |] [t| Abs.Exp |])

test5_1 = testEq "test 5.1" (simplProg program) (programSimpl)

data N = N M deriving (Show, Eq)
newtype M = M O deriving (Show, Eq)
type O = W
data W = W Int deriving (Show, Eq)

$(everywhere1 "ttt" 'id [t| N |] [t| Int |])

testMain :: IO ()
testMain = do 
    test1_1 >> test1_2
    test2_1
    test3_1
    test4_1 >> test4_2 >> test4_3
    test5_1
    -- putStrLn "Before tranformation" 
    -- print program
    -- putStrLn "After tranformation" 
    -- print $ simplProg program


