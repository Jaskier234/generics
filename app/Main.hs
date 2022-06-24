{-# language TemplateHaskell #-}
module Main where

import Generics
import Quotes
import DataGenerics
import Language.Haskell.TH

data A = A Int String [Int] | B Int | C String

-- $(evalEverywhere 'simpl [t| Prog |] [t| Expr |])

main :: IO ()
-- main = runQ (generateFunc 'id ''A) >>= putStrLn . pprint -- putStrLn fun
main = testMain -- do 
--     putStrLn "Before transformation"
--     print prog
--     putStrLn "After transformation"
--     print $ simplProg3 prog
-- main = putStrLn $ $(printToExp =<< execEverywhere 'succ [t| Prog |] [t| Int |])
-- main = print $(do 
--     mainType <- [t| Prog |]
--     transformType <- [t| Expr |]
--     mainQ mainType transformType)

