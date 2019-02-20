-- stack ghci --resolver lts-13.7 --package lens --package generic-lens
{-# LANGUAGE AllowAmbiguousTypes,
             DataKinds,
             DeriveGeneric,
             FlexibleInstances,
             FunctionalDependencies,
             KindSignatures,
             MultiParamTypeClasses,
             RankNTypes,
             ScopedTypeVariables,
             TypeApplications #-}
module UsingTheLensLibrary where

import Control.Lens -- lens
import Data.Generics.Product -- generic-lens
import Data.Generics.Sum -- generic-lens
import GHC.Generics -- base
import GHC.TypeLits -- base

data Customer =
    Customer { name :: String, age :: Int }
    deriving (Generic, Show)

data Payment
    = Cash { customer :: Customer
           , amount :: Double
           }
    | Credit { customer :: Customer
             , amount :: Double
             , cardNumber :: Int
             }
    | Check { customer :: Customer
            , amount :: Double
            , routingNumber :: Int
            , accountNumber :: Int
            }
    deriving (Generic, Show)

class HasPartialField (symbol :: Symbol) s a | symbol s -> a where
    fieldP :: Traversal' s a

instance HasPartialField "cardNumber" Payment Int where
    fieldP f (Credit cst amt cn) = fmap (\x -> Credit cst amt x) (f cn)
    fieldP _ pmt = pure pmt

instance HasPartialField "routingNumber" Payment Int where
    fieldP f (Check c a r n) = fmap (\x -> Check c a x n) (f r)
    fieldP _ pmt = pure pmt

instance HasPartialField "accountNumber" Payment Int where
    fieldP f (Check c a r n) = fmap (\x -> Check c a r x) (f n)
    fieldP _ pmt = pure pmt

daniel :: Customer
daniel = Customer "Daniel" 34

beersAtKingsHeadPub :: Payment
beersAtKingsHeadPub = Cash daniel 100
