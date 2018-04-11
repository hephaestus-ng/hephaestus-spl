{- |
Module      :  $Header$
Description :  An implementation of the main artifacts of an SPL
Copyright   :  (c) University of Bras\'{i}lia
License     :  <license>

Maintainer  :  rbonifacio@unb.br
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable

A proof-of-concept implementation of the main assets and functions
related to product line engineering. It includes:

  * definition for SPLs and SPL Instances
  * definition for configuration models
  * a generic product derivation functions (build)
  * a generic transformation for deriving products
  * a generic definition of a core asset

-}

module Data.SPL where

import Text.Parsec

import Data.FM.Types
import Data.FM.ProductConfiguration

class Asset a where
  initialize :: Product a
  parserT    :: Parsec String () (Transformation a)
  -- parserA    :: Parsec String () a
  -- export  :: a -> IO ()

data SPL a = SPL FeatureModel (ConfigurationKnowledge a)

data Product a = Product a
  deriving(Show)

instance Functor Product where
  fmap f (Product a) = Product (f a)


type Transformation a = SPL a -> Product a -> Product a

type ConfigurationKnowledge a = [(ConfigurationItem a)]

type ConfigurationItem a = (FeatureExp, [Transformation a])


build :: (Asset a) => SPL a -> ProductConfiguration -> Product a
build spl@(SPL fm ck) im = foldr (\t -> t spl) (initialize) ts
 where
   ts =  concat [t | (e, t) <- ck, isValidExp e im]
