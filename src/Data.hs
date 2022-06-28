{-# language DeriveDataTypeable #-}
{-# language RankNTypes #-}
module Data where

import Data.Typeable
import Data.Data

a :: Maybe Integer
a = cast (34 :: Integer)

b :: Maybe Integer
b = cast 'b'


ms :: Maybe Int -> Maybe Int
ms n = do 
  n' <- n 
  return $ n' + 1

s :: Int -> Int
s = succ

int :: Int -> Int
int n = n


mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT t = case cast t of
            Just t' -> t'
            Nothing -> id


data D = D Int Char [[Int]] deriving (Show, Data)

d = D 23 'f' [[44]]

gt :: D -> D
gt = gmapT $ mkT s

everywhere :: (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhere t = t . (gmapT (everywhere t))

