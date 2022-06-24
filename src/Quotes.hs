{-# language TemplateHaskell #-}
module Quotes
    ( someFunc
    , expr
    , decl
    ) where

import Language.Haskell.TH

someFunc :: IO ()
someFunc = (runQ decl) >>= print

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

expr :: Q Exp
expr = [| fib 20 |]

decl :: Q [Dec]
decl = [d|
  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-1) + fib (n-2) |]
