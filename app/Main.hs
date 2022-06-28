{-# language TemplateHaskell #-}
module Main where

import Generics
import Quotes
import Test
import Example
import Language.Haskell.TH

main :: IO ()
main = exampleMain
